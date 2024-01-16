// algorithm turning stuff into ints
//

#include <stdint.h>
#include <cmath>
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

using std::vector;

//
// Very cool hashing algorithm that I found thanks to Sebastian Krantz's `collapse`.
// The author of the hashing idea is Morgan Jacob methinks.
//
// The current implementation is very different from the previous authors
//

// to do:
// handle NAs in ints/double as int
// handle factors

enum {T_INT, T_DBL_INT, T_DBL, T_STR};

union double2int {
  double dbl;
  uint32_t uint[2];
};

inline int power_of_two(double x){
  return std::ceil(std::log2(x + 1));
}

inline uint32_t hash_single(uint32_t value, int shifter){
  return (3141592653U * value >> (32 - shifter));
}

inline uint32_t hash_double(uint32_t v1, uint32_t v2, int shifter){
  return (((3141592653U * v1) ^ (3141592653U * v2)) >> (32 - shifter));
}

// Class very useful to pass around the data on R vectors 
class r_vector {
  r_vector() = delete;
  
  SEXP x_conv;
  
public:
  r_vector(SEXP);
  
  // public properties
  int n;
  bool is_fast_int = false;
  int x_range = 0;
  int x_range_bin = 0;
  int x_min = 0;
  int type = 0;
  
  // this is only used in the quick ints algorithm
  // for factors and bool we assume there are NAs since we don't traverse the data
  //  to find the range, contrary to ints or dbl_ints
  bool any_na = true;
  int NA_value = -1;
  
  // pointers: only the valid one ends up non-null
  int *px_int = (int *) nullptr;
  double *px_dbl = (double *) nullptr;
  intptr_t *px_intptr = (intptr_t *) nullptr;
  
};

r_vector::r_vector(SEXP x){
  
  int n = Rf_length(x);
  this->n = n;
  
  bool IS_INT = false;
  if(Rf_isNumeric(x) || Rf_isFactor(x) || TYPEOF(x) == LGLSXP){
    
    if(TYPEOF(x) == REALSXP){
      // we check if the underlying structure is int
      this->px_dbl = REAL(x);
      IS_INT = true;
      double *px = REAL(x);
      double x_min = px[0], x_max = px[0], x_tmp;
      bool any_na = false;
      for(int i=1 ; i<n ; ++i){
        x_tmp = px[i];
        
        if(std::isnan(x_tmp)){
          any_na = true;
        } else if(!(x_tmp == (int) x_tmp)){
          IS_INT = false;
          break;
        } else if(x_tmp > x_max){
          x_max = x_tmp;
        } else if(x_tmp < x_min){
          x_min = x_tmp;
        }
      }
      
      this->any_na = any_na;

      this->x_min = static_cast<int>(x_min);
      // +1 for the NAs
      this->x_range = x_max - x_min + 1;
      
      type = IS_INT ? T_DBL_INT : T_DBL;
    } else {
      // logical, factor and integer are all integers
      IS_INT = true;
      this->px_int = INTEGER(x);
      type = T_INT;
      
      if(TYPEOF(x) == INTSXP){
        int *px = INTEGER(x);
        int x_min = px[0], x_max = px[0], x_tmp;
        bool any_na = false;
        for(int i=1 ; i<n ; ++i){
          x_tmp = px[i];
          
          if(x_tmp == NA_INTEGER){
            any_na = true;
          } else if(x_tmp > x_max){
            x_max = x_tmp;
          } else if(x_tmp < x_min){
            x_min = x_tmp;
          }
        }
        
        this->any_na = any_na;
        this->x_min = x_min;
        // +1 for the NAs
        this->x_range = x_max - x_min + 1;
      } else if(TYPEOF(x) == LGLSXP){
        this->x_min = 0;
        // 0, 1, NA
        this->x_range = 3;
      } else {
        // factor
        SEXP labels = Rf_getAttrib(x, R_LevelsSymbol);
        // factors always start at 1
        this->x_min = 1;
        // we add 1 for the NAs
        this->x_range = Rf_length(labels) + 1;
      }
    }
    
  } else if(TYPEOF(x) == STRSXP){
    type = T_STR;
    this->px_intptr = (intptr_t *) STRING_PTR(x);
  } else {
    
    if(TYPEOF(x) == CHARSXP || TYPEOF(x) == LGLSXP || TYPEOF(x) == INTSXP || 
       TYPEOF(x) == REALSXP || TYPEOF(x) == CPLXSXP || TYPEOF(x) == STRSXP || TYPEOF(x) == RAWSXP){
      // we convert to character
      SEXP call_as_character = PROTECT(Rf_lang2(Rf_install("as.character"), x));
  
      int any_error;
      this->x_conv = R_tryEval(call_as_character, R_GlobalEnv, &any_error);

      if(any_error){
        Rf_error("In `to_index`, the vector to index was not standard (int or real, etc) and failed to be converted to character before applying indexation._n");
      }
      
      UNPROTECT(1);
      
      // conversion succeeded
      type = T_STR;
      this->px_intptr = (intptr_t *) STRING_PTR(this->x_conv);
      
    } else {
      Rf_error("In `to_index`, the R vectors must be atomic. The current type is not valid.");
    }    
    
  }
  
  if(IS_INT){
    // finding out if we're in the easy case
    this->x_range_bin = power_of_two(this->x_range);    
    this->is_fast_int = this->x_range < 100000 || this->x_range_bin <= power_of_two(2.5*n);
    this->NA_value = this->x_range - 1;
  }  
}

SEXP std_string_to_r_string(std::vector<std::string> x){
  
  int n = x.size();
  
  SEXP res = PROTECT(Rf_allocVector(STRSXP, n));
  
  for(int i=0 ; i<n ; ++i){
    SET_STRING_ELT(res, i, Rf_mkCharCE(x[i].c_str(), CE_UTF8));
  }
  
  UNPROTECT(1);
  
  return res;
}

void general_type_to_index_single(r_vector *x, int *__restrict p_index, int &n_groups,
                                  vector<int> &vec_first_obs, bool is_final){
  
  const size_t n = x->n;
  
  // we hash the vectors successively to turn them into "sparse" int32
  // we combine values from different vectors with xor
  // we cut the hashed value to fit into 2**(log2(n + 1))
  // we fill the group vector and check for collision all the time (this is costly)
  
  
  // we find out the number of bits (see shifter)
  // we find the first multiple of 2 greater than n
  int shifter = power_of_two(2.0 * n + 1.0);
  if(shifter < 8) shifter = 8;
  size_t larger_n = std::pow(2, shifter);
  
  // hashed_obs_vec:
  // - assume hash(value) leads to ID
  // - then hashed_obs_vec[ID] is the observation id of the first observation with that hash
  // - note that using an array makes the algo twice faster
  int *hashed_obs_vec = new int[larger_n];
  std::fill_n(hashed_obs_vec, larger_n, 0);
  
  const int *px_int = (int *) x->px_int;
  const double *px_dbl = (double *) x->px_dbl;
  const intptr_t *px_intptr = (intptr_t *) x->px_intptr;
  
  const int x_type = x->type;
  
  union double2int u_d2int;
  int g = 0;
  uint32_t id = 0;
  int obs = 0;
  if(x_type == T_STR){
    for(size_t i=0 ; i<n ; ++i){
      
      id = hash_single(px_intptr[i] & 0xffffffff, shifter);
      
      bool does_exist = false;
      while(hashed_obs_vec[id] != 0){
        obs = hashed_obs_vec[id] - 1;
        if(px_intptr[obs] == px_intptr[i]){
          p_index[i] = p_index[obs];
          does_exist = true;
          break;
        } else {
          ++id;
          if(id > larger_n){
            id %= larger_n;
          }
        }
      }

      if(!does_exist){
        // hash never seen => ok
        hashed_obs_vec[id] = i + 1;
        p_index[i] = ++g;
        if(is_final){
          vec_first_obs.push_back(i + 1);
        }
      }
    }
  } else if(x_type == T_INT){
    for(size_t i=0 ; i<n ; ++i){
      id = hash_single(px_int[i], shifter);
      
      bool does_exist = false;
      while(hashed_obs_vec[id] != 0){
        obs = hashed_obs_vec[id] - 1;
        if(px_int[obs] == px_int[i]){
          p_index[i] = p_index[obs];
          does_exist = true;
          break;
        } else {
          ++id;
          if(id > larger_n){
            id %= larger_n;
          }
        }
      }

      if(!does_exist){
        hashed_obs_vec[id] = i + 1;
        p_index[i] = ++g;
        if(is_final){
          vec_first_obs.push_back(i + 1);
        }
      }
    }
  } else {
    // NOTA: the compiler will take the if() out of the loop
    // not possible if we also have an if() inside the while() loop
    // => explains why I needed to repeat all the for loops
    for(size_t i=0 ; i<n ; ++i){
      
      if(x_type == T_DBL_INT){
        id = hash_single((int) px_dbl[i], shifter);
      } else {
        u_d2int.dbl = px_dbl[i];
        id = hash_single(u_d2int.uint[0] + u_d2int.uint[1], shifter);
      }      
      
      bool does_exist = false;
      while(hashed_obs_vec[id] != 0){
        obs = hashed_obs_vec[id] - 1;
        if(px_dbl[obs] == px_dbl[i]){
          p_index[i] = p_index[obs];
          does_exist = true;
          break;
        } else {
          ++id;
          if(id > larger_n){
            id %= larger_n;
          }
        }
      }

      if(!does_exist){
        hashed_obs_vec[id] = i + 1;
        p_index[i] = ++g;
        if(is_final){
          vec_first_obs.push_back(i + 1);
        }
      }
    }
  }
  
  n_groups = g;
  delete[] hashed_obs_vec;
  
}

void general_type_to_index_double(r_vector *x, int *__restrict p_index_in, 
                                  int *__restrict p_index_out, int &n_groups,
                                  vector<int> &vec_first_obs, bool is_final){
  // Two differences with the *_single version:
  // - when hashing and checking for collision => we use the extra index
  // - we include the possibility of fast ints
  //
  
  // general information
  const size_t n = x->n;
  
  const int *px_int = (int *) x->px_int;
  const double *px_dbl = (double *) x->px_dbl;
  const intptr_t *px_intptr = (intptr_t *) x->px_intptr;
  
  const int x_type = x->type;
  int g = 0;
  
  bool do_fast_int = false;
  if(x->is_fast_int){
    int sum_range_bin = x->x_range_bin + power_of_two(n_groups);
    do_fast_int = sum_range_bin < 17 || sum_range_bin <= power_of_two(5.0 * n);
  }
  
  if(do_fast_int){
    
    int n_groups_bin = power_of_two(n_groups);
    size_t lookup_size = std::pow(2, x->x_range_bin + n_groups_bin + 1);
    int *int_array = new int[lookup_size];
    std::fill_n(int_array, lookup_size, 0);
    
    const bool is_x_int = x_type == T_INT;
    const int x_min = x->x_min;
    const int offset = n_groups_bin;
    int id = 0;      
    for(size_t i=0 ; i<n ; ++i){
      if(is_x_int){
        id = p_index_in[i] + ((px_int[i] - x_min) << offset);
      } else {
        id = p_index_in[i] + ((static_cast<int>(px_dbl[i]) - x_min) << offset);
      }      
      
      if(int_array[id] == 0){
        ++g;
        int_array[id] = g;
        p_index_out[i] = g;
        if(is_final){
          vec_first_obs.push_back(i + 1);
        }
      } else {
        p_index_out[i] = int_array[id];
      }
    }
    
  } else {  
    // we hash the vectors successively to turn them into "sparse" int32
    // we combine values from different vectors with xor
    // we cut the hashed value to fit into 2**(log2(n + 1))
    // we fill the group vector and check for collision all the time (this is costly)
    
    // we find out the number of bits (see shifter)
    // we find the first multiple of 2 greater than n
    int shifter = power_of_two(2.0 * n + 1.0);
    if(shifter < 8) shifter = 8;
    size_t larger_n = std::pow(2, shifter);
    
    // hashed_obs_vec:
    // - assume hash(value) leads to ID
    // - then hashed_obs_vec[ID] is the observation id of the first observation with that hash
    // - note that using an array makes the algo twice faster
    int *hashed_obs_vec = new int[larger_n];
    std::fill_n(hashed_obs_vec, larger_n, 0);
    
    union double2int u_d2int;
    uint32_t id = 0;
    int obs = 0;
    if(x_type == T_STR){
      for(size_t i=0 ; i<n ; ++i){
        
        id = hash_double(px_intptr[i] & 0xffffffff, p_index_in[i], shifter);
        
        bool does_exist = false;
        while(hashed_obs_vec[id] != 0){
          obs = hashed_obs_vec[id] - 1;
          if(px_intptr[obs] == px_intptr[i] && p_index_in[obs] == p_index_in[i]){
            p_index_out[i] = p_index_out[obs];
            does_exist = true;
            break;
          } else {
            ++id;
            if(id > larger_n){
              id %= larger_n;
            }
          }
        }

        if(!does_exist){
          // hash never seen => ok
          hashed_obs_vec[id] = i + 1;
          p_index_out[i] = ++g;
          if(is_final){
            vec_first_obs.push_back(i + 1);
          }
        }
      }
    } else if(x_type == T_INT){
      for(size_t i=0 ; i<n ; ++i){
        id = hash_double(px_int[i], p_index_in[i], shifter);
        
        bool does_exist = false;
        while(hashed_obs_vec[id] != 0){
          obs = hashed_obs_vec[id] - 1;
          if(px_int[obs] == px_int[i] && p_index_in[obs] == p_index_in[i]){
            p_index_out[i] = p_index_out[obs];
            does_exist = true;
            break;
          } else {
            ++id;
            if(id > larger_n){
              id %= larger_n;
            }
          }
        }

        if(!does_exist){
          hashed_obs_vec[id] = i + 1;
          p_index_out[i] = ++g;
          if(is_final){
            vec_first_obs.push_back(i + 1);
          }
        }
      }
    } else {
      // NOTA: the compiler will take the if() out of the loop
      // not possible if we also have an if() inside the while() loop
      // => explains why I needed to repeat all the for loops
      for(size_t i=0 ; i<n ; ++i){
        
        if(x_type == T_DBL_INT){
          id = hash_double((int) px_dbl[i], p_index_in[i], shifter);
        } else {
          u_d2int.dbl = px_dbl[i];
          id = hash_double(u_d2int.uint[0] + u_d2int.uint[1], p_index_in[i], shifter);
        }      
        
        bool does_exist = false;
        while(hashed_obs_vec[id] != 0){
          obs = hashed_obs_vec[id] - 1;
          if(px_dbl[obs] == px_dbl[i] && p_index_in[obs] == p_index_in[i]){
            p_index_out[i] = p_index_out[obs];
            does_exist = true;
            break;
          } else {
            ++id;
            if(id > larger_n){
              id %= larger_n;
            }
          }
        }

        if(!does_exist){
          hashed_obs_vec[id] = i + 1;
          p_index_out[i] = ++g;
          if(is_final){
            vec_first_obs.push_back(i + 1);
          }
        }
      }
    }
    
    delete[] hashed_obs_vec;
  }
  
  n_groups = g;
}
inline void update_index_intarray_g_obs(int id, size_t i, int &g, int * &int_array, 
                                        int * &p_index, bool &is_final, vector<int> &vec_first_obs){
  
  if(int_array[id] == 0){
    ++g;
    int_array[id] = g;
    p_index[i] = g;
    if(is_final){
      vec_first_obs.push_back(i + 1);
    }
  } else {
    p_index[i] = int_array[id];
  }  
}

void multiple_ints_to_index(vector<r_vector> &all_vecs, vector<int> &all_k, 
                            int *__restrict p_index, int &n_groups,
                            vector<int> &vec_first_obs, bool is_final){
  
  int sum_bin_ranges = 0;
  int K = all_k.size();
    
  for(auto &&k : all_k){
    sum_bin_ranges += all_vecs[k].x_range_bin;
  }  
  
  int k0 = all_k[0];
  r_vector *x0 = &all_vecs[k0];
  const size_t n = x0->n;
  const int * px0_int = (int *) x0->px_int;
  const double * px0_dbl = (double *) x0->px_dbl;
  
  const int x0_type = x0->type;
  const bool is_x0_int = x0_type == T_INT;
  const int x0_min = x0->x_min;
  
  size_t lookup_size = K == 1 ? x0->x_range + 1 : std::pow(2, sum_bin_ranges + K - 1);
  int *int_array = new int[lookup_size];
  std::fill_n(int_array, lookup_size, 0);
  
  int g = 0;                    
  
  if(K == 1){
    int id = 0;
    
    if(is_x0_int){
      if(x0->any_na){
        int NA_value = x0->NA_value;
        for(size_t i=0 ; i<n ; ++i){
          if(px0_int[i] == NA_INTEGER){
            id = NA_value;
          } else {
            id = px0_int[i] - x0_min;
          }
          
          update_index_intarray_g_obs(id, i, g, int_array, p_index, is_final, vec_first_obs);
        }
      } else {
        for(size_t i=0 ; i<n ; ++i){
          id = px0_int[i] - x0_min;
          update_index_intarray_g_obs(id, i, g, int_array, p_index, is_final, vec_first_obs);
        }
      }
    } else {
      // Double as int
      if(x0->any_na){
        int NA_value = x0->NA_value;
        for(size_t i=0 ; i<n ; ++i){
          if(std::isnan(px0_dbl[i])){
            id = NA_value;
          } else {
            id = static_cast<int>(px0_dbl[i]) - x0_min;
          }
          
          update_index_intarray_g_obs(id, i, g, int_array, p_index, is_final, vec_first_obs);
        }
      } else {
        for(size_t i=0 ; i<n ; ++i){
          id = static_cast<int>(px0_dbl[i]) - x0_min;
          update_index_intarray_g_obs(id, i, g, int_array, p_index, is_final, vec_first_obs);
        }
      }
    }
  } else {
    
    int k1 = all_k[1];
    r_vector *x1 = &all_vecs[k1];
    const int *px1_int = (int *) x1->px_int;
    const double *px1_dbl = (double *) x1->px_dbl;
    
    const bool x1_type = x1->type;
    const bool is_x1_int = x1_type == T_INT;   
    const int x1_min = x1->x_min;
    
    if(K == 2){
      int id = 0;
      int offset = x0->x_range_bin;
      int v0 = 0, v1 = 0;
      for(size_t i=0 ; i<n ; ++i){
        
        v0 = is_x0_int ? px0_int[i] - x0_min : static_cast<int>(px0_dbl[i]) - x0_min;
        v1 = is_x1_int ? px1_int[i] - x1_min : static_cast<int>(px1_dbl[i]) - x1_min;
        
        id = v0 + (v1 << offset);
        
        if(int_array[id] == 0){
          ++g;
          int_array[id] = g;
          p_index[i] = g;
          if(is_final){
            vec_first_obs.push_back(i + 1);
          }
        } else {
          p_index[i] = int_array[id];
        }
      }
    } else {
      int *sum_vec = new int[n];
      
      // we first initialize sum_vec to the sum of the two first elements
      
      int offset = x0->x_range_bin;
      int v0 = 0, v1 = 0;
      bool any_na0 = x0->any_na, any_na1 = x1->any_na;
      int NA_value0 = x0->NA_value, NA_value1 = x1->NA_value;
      // I think the ifs below can be unrolled by the compiler
      for(size_t i=0 ; i<n ; ++i){
        if(is_x0_int){
          if(any_na0){
            if(px0_int[i] == NA_INTEGER){
              v0 = NA_value0;
            } else {
              v0 = px0_int[i] - x0_min;
            }
          } else {
            v0 = px0_int[i] - x0_min;
          }
        } else {
          if(any_na0){
            if(std::isnan(px0_dbl[i])){
              v0 = NA_value0;
            } else {
              v0 = static_cast<int>(px0_dbl[i]) - x0_min;
            }
          } else {
            v0 = static_cast<int>(px0_dbl[i]) - x0_min;
          }
        }
        
        if(is_x1_int){
          if(any_na1){
            if(px1_int[i] == NA_INTEGER){
              v1 = NA_value1;
            } else {
              v1 = px1_int[i] - x1_min;
            }
          } else {
            v1 = px1_int[i] - x1_min;
          }
        } else {
          if(any_na1){
            if(std::isnan(px1_dbl[i])){
              v1 = NA_value1;
            } else {
              v1 = static_cast<int>(px1_dbl[i]) - x1_min;
            }
          } else {
            v1 = static_cast<int>(px1_dbl[i]) - x1_min;
          }
        }
        
        sum_vec[i] = v0 + (v1 << offset);
      }
      
      // we sum
      offset += x1->x_range_bin;
      for(int ind=2 ; ind<K-1 ; ++ind){
        int k = all_k[ind];
        r_vector *x = &all_vecs[k];
        const int *px_int = (int *) x->px_int;
        const double *px_dbl = (double *) x->px_dbl;
        
        const int x_type = x->type;
        const bool is_int = x_type == T_INT;
        const int x_min = x->x_min;
        int vk = 0;
        bool any_na = x->any_na;
        int NA_value = x->NA_value;
        for(size_t i=0 ; i<n ; ++i){
          // vk = is_int ? px_int[i] - x_min : static_cast<int>(px_dbl[i]) - x_min;
          if(is_int){
            if(any_na){
              if(px_int[i] == NA_INTEGER){
                vk = NA_value;
              } else {
                vk = px_int[i] - x_min;
              }
            } else {
              vk = px_int[i] - x_min;
            }
          } else {
            if(any_na){
              if(std::isnan(px_dbl[i])){
                vk = NA_value;
              } else {
                vk = static_cast<int>(px_dbl[i]) - x_min;
              }
            } else {
              vk = static_cast<int>(px_dbl[i]) - x_min;
            }
          }
          sum_vec[i] += (vk << offset);
        }
        offset += x->x_range_bin;
      }
            
      // last element + group creation
      int k = all_k[K - 1];
      r_vector *x = &all_vecs[k];
      const int *px_int = (int *) x->px_int;
      const double *px_dbl = (double *) x->px_dbl;
      
      const int x_type = x->type;
      const bool is_int = x_type == T_INT;
      const int x_min = x->x_min;
      
      int id = 0;
      int vk = 0;
      bool any_na = x->any_na;
      int NA_value = x->NA_value;
      for(size_t i=0 ; i<n ; ++i){
        // vk = is_int ? px_int[i] - x_min : static_cast<int>(px_dbl[i]) - x_min;
        if(is_int){
          if(any_na){
            if(px_int[i] == NA_INTEGER){
              vk = NA_value;
            } else {
              vk = px_int[i] - x_min;
            }
          } else {
            vk = px_int[i] - x_min;
          }
        } else {
          if(any_na){
            if(std::isnan(px_dbl[i])){
              vk = NA_value;
            } else {
              vk = static_cast<int>(px_dbl[i]) - x_min;
            }
          } else {
            vk = static_cast<int>(px_dbl[i]) - x_min;
          }
        }        
        
        id = sum_vec[i] + (vk << offset);
        
        if(int_array[id] == 0){
          // new item: we save the group id
          ++g;
          int_array[id] = g;
          p_index[i] = g;
          if(is_final){
            vec_first_obs.push_back(i + 1);
          }
        } else {
          p_index[i] = int_array[id];
        }
      }
      
      delete[] sum_vec;
    }
  }
  
  n_groups = g;
  
  delete[] int_array;
}

// [[Rcpp::export]]
SEXP cpp_to_index_bis(SEXP x){
  // x: vector or list of vectors of the same length (n)
  // returns:
  // - index: vector of length n, from 1 to the numberof unique values of x (g)
  // - first_obs: vector of length g of the first observation belonging to each group
  
  size_t n = 0;
  int K = 0;
  std::vector<r_vector> all_vecs;
  
  // we set up the info with the rvec class. It makes it easy to pass across functions
  if(TYPEOF(x) == VECSXP){
    K = Rf_length(x);
    for(int k=0; k<K; ++k){
      r_vector rvec(VECTOR_ELT(x, k));
      all_vecs.push_back(rvec);
    }
    n = Rf_length(VECTOR_ELT(x, 0));
  } else {
    K = 1;
    n = Rf_length(x);
    r_vector rvec(x);
    all_vecs.push_back(rvec);
  }
  
  // the result to be returned
  SEXP index = PROTECT(Rf_allocVector(INTSXP, n));
  int *p_index = INTEGER(index);
  
  // vector of the first observation of the group
  std::vector<int> vec_first_obs;
  
  // finding out the fast cases
  // Note that partial fast ordering is enabled and 
  // we stop at the first feasible possibility
  int sum_bin_ranges = 0;
  vector<int> id_fast_int;
  for(int k=0 ; k<K ; ++k){
    r_vector *x = &all_vecs[k];
    if(x->is_fast_int){
      int new_bin_range = sum_bin_ranges + x->x_range_bin;
      if(new_bin_range < 17 || (K >= 2 && new_bin_range <= power_of_two(5 * n))){
        id_fast_int.push_back(k);
        sum_bin_ranges = new_bin_range;
      } else {
        break;
      }      
    }
  }
  
  int n_groups;
  
  //
  // STEP 1: taking care of fast indexing of ints
  //
  
  bool is_final = false;
  bool init_done = false;
  if(!id_fast_int.empty()){
    init_done = true;
    
    is_final = (size_t) K == id_fast_int.size();
    multiple_ints_to_index(all_vecs, id_fast_int, p_index, n_groups, vec_first_obs, is_final);
  }
  
  if(!is_final){
    
    // 
    // STEP 2: general algorithm
    //
    
    // note: here only if not all vectors are "fast"
    //  
    
    // first we find out who is left
    vector<int> all_k_left;
    for(int k=0 ; k<K ; ++k){
      if(std::find(id_fast_int.begin(), id_fast_int.end(), k) == id_fast_int.end()){
        // i.e. if k not in id_fast_int (which has been done)
        all_k_left.push_back(k);
      }
    }
    
    if(!init_done){
      int k0 = all_k_left[0];
      // we remove that element
      all_k_left.erase(all_k_left.begin());
      
      is_final = all_k_left.empty();
      general_type_to_index_single(&all_vecs[k0], p_index, n_groups, vec_first_obs, is_final);
    }
    
    if(!is_final){
      // here: p_index is an index
      // we will loop over all remainining items and create the index sequentially
      
      int *p_extra_index = new int[n];
      
      bool is_res_updated_index = true;
      for(size_t ind=0 ; ind<all_k_left.size() ; ++ind){
        int k = all_k_left[ind];
        is_final = ind == all_k_left.size() - 1;
        if(is_res_updated_index){
          general_type_to_index_double(&all_vecs[k], p_index, p_extra_index, n_groups, vec_first_obs, is_final);
          is_res_updated_index = false;
        } else {
          general_type_to_index_double(&all_vecs[k], p_extra_index, p_index, n_groups, vec_first_obs, is_final);
          is_res_updated_index = true;
        }
      }
      
      if(!is_res_updated_index){
        std::memcpy(p_index, p_extra_index, sizeof(int) * n);
      }
      delete[] p_extra_index;
    }
  } 
  
  // we copy the first observations into an R vector
  int g = vec_first_obs.size();
  SEXP r_first_obs = PROTECT(Rf_allocVector(INTSXP, g));
  int *p_first_obs = INTEGER(r_first_obs);
  std::memcpy(p_first_obs, vec_first_obs.data(), sizeof(int) * g);
  
  // we save the results into a list
  SEXP res = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(res, 0, index);
  SET_VECTOR_ELT(res, 1, r_first_obs);
  
  // names
  Rf_setAttrib(res, R_NamesSymbol, std_string_to_r_string({"index", "first_obs"}));
    
  UNPROTECT(3);
  
  return res;  
}


class double_char_vec {
  intptr_t * p_x1 = nullptr;
  intptr_t * p_x2 = nullptr;
  
  int n1 = 0;
  int n2 = 0;
  
  public:
  
  double_char_vec() = delete;
  double_char_vec(SEXP, SEXP);
  
  int operator[](int i){
    return i < n1 ? p_x1[i] : p_x2[i - n1];
  }
  
  int size(){
    return n1 + n2;
  }
};

double_char_vec::double_char_vec(SEXP x1, SEXP x2){
  n1 = Rf_length(x1);
  n2 = Rf_length(x2);
  
  p_x1 = (intptr_t *) STRING_PTR(x1);
  p_x2 = (intptr_t *) STRING_PTR(x2);
}


// [[Rcpp::export]]
SEXP cpp_double_factor_to_index(SEXP x1, SEXP x2){
  // x1, x2: factors
  // returns an index vector of size len(x1) + len(x2)
  
  if(!Rf_isFactor(x1)){
    Rf_error("The argument x1 must be a factor.");
  }
  
  if(!Rf_isFactor(x2)){
    Rf_error("The argument x2 must be a factor.");
  }
  
  SEXP lab1 = Rf_getAttrib(x1, R_LevelsSymbol); 
  SEXP lab2 = Rf_getAttrib(x2, R_LevelsSymbol); 
  
  //
  // STEP 1: we turn the set of all labels into an index
  //
  
  double_char_vec all_labels(lab1, lab2);
  
  int n_labels = all_labels.size();
  
  // this algorithm is taken from general_type_to_index_single
  int shifter = power_of_two(2.0 * n_labels + 1.0);
  if(shifter < 8) shifter = 8;
  size_t larger_n = std::pow(2, shifter);
  
  int *hashed_obs_vec = new int[larger_n];
  std::fill_n(hashed_obs_vec, larger_n, 0);
  
  int *p_lab_index = new int[n_labels];
  
  int g = 0;
  uint32_t id = 0;
  int obs = 0;
  for(int i=0 ; i<n_labels ; ++i){
    id = hash_single(all_labels[i] & 0xffffffff, shifter);
    
    bool does_exist = false;
    while(hashed_obs_vec[id] != 0){
      obs = hashed_obs_vec[id] - 1;
      if(all_labels[obs] == all_labels[i]){
        p_lab_index[i] = p_lab_index[obs];
        does_exist = true;
        break;
      } else {
        ++id;
        if(id > larger_n){
          id %= larger_n;
        }
      }
    }

    if(!does_exist){
      // hash never seen => ok
      hashed_obs_vec[id] = i + 1;
      p_lab_index[i] = ++g;
    }
  }
  
  
  //
  // STEP 2: we turn the two input factors into a single index 
  //
  
  
  int n1 = Rf_length(x1);
  int n2 = Rf_length(x2);
  
  int *p_x1 = INTEGER(x1);
  int *p_x2 = INTEGER(x2);
  
  SEXP index = PROTECT(Rf_allocVector(INTSXP, n1 + n2));
  int *p_index = INTEGER(index);
  
  int g_NA = ++g;
  int g_MAX = g;
  
  int *p_g_exists = new int[g_MAX];
  std::fill_n(p_g_exists, g_MAX, 0);
  
  int xi = 0;
  for(int i=0 ; i<n1 ; ++i){
    xi = p_x1[i];
    if(xi == NA_INTEGER){
      p_g_exists[g_NA - 1] = 1;
      p_index[i] = g_NA;
    } else {
      g = p_lab_index[xi - 1];
      p_g_exists[g - 1] = 1;
      p_index[i] = g;
    }    
  }
  
  int *p_lab_index_2 = p_lab_index + Rf_length(lab1);
  for(int i=0 ; i<n2 ; ++i){
    xi = p_x2[i];
    if(xi == NA_INTEGER){
      p_g_exists[g_NA - 1] = 1;
      p_index[n1 + i] = g_NA;
    } else {
      g = p_lab_index_2[xi - 1];
      p_g_exists[g - 1] = 1;
      p_index[n1 + i] = g;
    }
  }
  
  delete[] p_lab_index;
  
  //
  // STEP 3: re-index the index if there are more labels than values
  //
  
  bool reindex = false;
  for(int i=0 ; i<g_MAX ; ++i){
    if(p_g_exists[i] == 0){
      if(i < g_NA - 1){
        // if only NAs are not there, we don't reindex
        // otherwise we do
        reindex = true;
      }
      break;
    }
  }
  
  delete[] p_g_exists;
  
  if(reindex){
    
    int *int_array = new int[g_MAX];
    std::fill_n(int_array, g_MAX, 0);
    
    int g = 0;
    for(int i=0 ; i<n1+n2 ; ++i){
      id = p_index[i] - 1;      
      
      if(int_array[id] == 0){
        // new item: we save the group id
        ++g;
        int_array[id] = g;
        p_index[i] = g;
      } else {
        p_index[i] = int_array[id];
      }
    }
    
    delete[] int_array;
  }
  
  UNPROTECT(1);
  return index;
}



