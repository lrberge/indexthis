#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2024-01-17
# ~: Main tests
#------------------------------------------------------------------------------#

# R -d "valgrind --leak-check=full"

# Rcpp::sourceCpp("src/to_index.cpp")
# source("R/to_index.R") ; source("R/test_funs.R")
# library(dreamerr)

n = 500

words = paste0(rep(letters, 5), rep(letters, each = 5), rep(rev(letters), 5))

years = 1800:2023
months = 1:12
day = 1:28
date_sample = as.Date(paste0(sample(years, 200, TRUE), "-", 
                             sample(months, 200, TRUE), "-", 
                             sample(day, 200, TRUE)))


set.seed(1)
base = list(
  int = as.integer(rnorm(n, sd = 8)),
  fact = factor(sample(letters[-(1:5)], n, TRUE), letters),
  bool = sample(c(TRUE, FALSE), n, TRUE),
  dbl_int = round(rnorm(n, sd = 8)),
  dbl = round(rnorm(n, sd = 8), 1),
  char = sample(words, n, TRUE),
  date = sample(date_sample, n, TRUE),
  complex = complex(real = round(rnorm(n, sd = 4)), imaginary = round(rnorm(n, sd = 4)))
)

####
#### single vector ####
####

for(i_type in seq_along(base)){
  cat(format(names(base))[i_type])
  x = base[[i_type]]
  for(any_na in c(FALSE, TRUE)){
    cat(".")
    if(any_na){
      x[c(1, 32, 65, 125)] = NA
    }
    
    index = to_index(x)
    
    x_char = as.character(x)
    if(any_na){
      x_char[is.na(x_char)] = "NA"
    }
    index_r = unclass(as.factor(x_char))
    
    test(nrow(unique(data.frame(index, index_r))), max(index))
  }
  cat("\n")
}



####
#### double vector ####
####

for(i_type in seq_along(base)){
  cat(format(names(base))[i_type])
  x_raw = base[[i_type]]
  for(j_type in seq_along(base)){
    cat("\n  ", format(names(base))[j_type])
    y_raw = base[[j_type]]
    for(any_na in c(FALSE, TRUE)){
      cat(".")
      x = x_raw
      y = y_raw
      if(any_na){
        x[c(1, 32, 65, 125)] = NA
        y[c(1, 32)] = y[1]
        
        y[c(2, 33, 65, 200, 225)] = NA
        x[c(2, 33)] = x[2]
      }
      
      index = to_index(x, y)
      
      x_char = paste0(x, "_", y)
      index_r = unclass(as.factor(x_char))
      
      test(nrow(unique(data.frame(index, index_r))), max(index))
    }
  }
  cat("\n")
}

####
#### triple vector ####
####

for(i_type in seq_along(base)){
  
  cat(format(names(base))[i_type])
  x_raw = base[[i_type]]
  
  for(j_type in seq_along(base)){
    
    cat("\n  ", format(names(base))[j_type])
    y_raw = base[[j_type]]
    
    for(k_type in seq_along(base)){
      
      cat("\n    ", format(names(base))[k_type])
      z_raw = base[[k_type]]
      
      for(any_na in c(FALSE, TRUE)){
        cat(".")
        
        x = x_raw
        y = y_raw
        z = z_raw
        
        if(any_na){
          x[c(1, 32, 65, 125)] = NA
          y[c(2, 33, 67, 200)] = NA
          z[c(8, 33, 50, 200)] = NA
        }
        
        index = to_index(x, y, z)
        
        x_char = paste0(x, "_", y, "_", z)
        index_r = unclass(as.factor(x_char))
        
        test(nrow(unique(data.frame(index, index_r))), max(index))
      }
    }
  }
  cat("\n")
}


####
#### quadruple vectors ####
####

int_types = 1:4
for(i_type in int_types){
  
  x_raw = base[[i_type]]
  
  for(j_type in int_types){
    
    y_raw = base[[j_type]]
    
    for(k_type in int_types){
      
      z_raw = base[[k_type]]
      
      for(l_type in int_types){
        
        cat("\n", 
            format(names(base))[i_type], ", ",
            format(names(base))[j_type], ", ",
            format(names(base))[k_type], ", ",
            format(names(base))[l_type])
        zz_raw = base[[l_type]]
        
        for(any_na in c(FALSE, TRUE)){
          cat(".")
          
          x = x_raw
          y = y_raw
          z = z_raw
          zz = zz_raw
          
          if(any_na){
            x[c(1, 32, 65, 125)] = NA
            y[c(2, 33, 67, 200)] = NA
            z[c(8, 33, 50, 200)] = NA
            zz[c(8, 33, 50, 200)] = NA
          }
          
          index = to_index(x, y, z, zz)
          
          x_char = paste0(x, "_", y, "_", z, "_", zz)
          index_r = unclass(as.factor(x_char))
          
          test(nrow(unique(data.frame(index, index_r))), max(index))
        }
      }
    }
  }
  cat("\n--------------------------------------------\n")
}


####
#### edge cases ####
####

base_na = list(
  int = rep(NA_integer_, 50),
  dbl = rep(NA_real_, 50),
  char = rep(NA_character_, 50),
  complex = rep(NA_complex_, 50)  
)

for(i in seq_along(base_na)){
  index = to_index(base_na[[i]])
  test(all(index == index[1]), TRUE)
}

for(i in seq_along(base_na)){
  for(j in seq_along(base_na)){
    index = to_index(base_na[[i]], base_na[[j]])
    test(all(index == index[1]), TRUE)
  }
}



