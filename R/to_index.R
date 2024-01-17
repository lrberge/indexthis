#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2024-01-11
# ~: main index function
#------------------------------------------------------------------------------#


#' Turns one or multiple vectors into an index (aka group id, aka key)
#' 
#' Turns one or multiple vectors of the same length into an index, that is a integer vector 
#' of the same length ranging from 1 to the number of unique elements in the vectors. 
#' This is equivalent to creating a key.
#' 
#' @param ... The vectors to be turned into indexes. Only works for atomic vectors. 
#' If multiple vectors are provided, they should all be of the same length. Notes that 
#' you can alternatively provide a list of vectors with the argument `list`.
#' @param list An alternative to using `...` to pass the input vectors. If provided, it
#' should be a list of atomic vectors, all of the same length. If this argument is provided,
#' then `...` is ignored.
#' @param sorted Logical, default is `FALSE`. By default the index order is based on 
#' the order of occurence. Values occurring before have lower index values. Use `sorted=TRUE`
#' to have the index to be sorted based on the vector values. For example `c(7, 3, 7, -8)` will be 
#' turned into `c(1, 2, 1, 3)` if sorted=FALSE and into `c(3, 2, 3, 1)` is `sorted=TRUE`.
#' @param items.out Logical, default is `FALSE`. Whether to return the input values the indexes
#' refer to. If `TRUE`, the attribute `"items"` is created with the vector values. Note that
#' you can return instead a list insteda of an attribute with `out.list = TRUE`.
#' @param out.list Logical, default is `FALSE`. If `TRUE`, the function returns a list 
#' of two elements: `index` and `items`. The `items` is the unique input elements 
#' the index refers to.
#' @param items.df Logical, default is `FALSE`. Whether to return the input elements (to which the index refer) in the form of a data.frame.
#' @param items.join Character scalar, default is `"_"`. Only used if the items are returned and 
#' there were multiple vectors as input and if `items.df=FALSE`. If there are multiple 
#' vectors in input, their unique elements are joined with `items.join`, so that a single
#' character vector represent their combination.
#' @param internal Logical, default is `FALSE`. If `TRUE`, some checks on the data are ignored.
#' 
#' @author 
#' Laurent Berge
#' 
#' 
#' @examples
#' 
#' x = c("u", "a", "a", "s", "u", "u")
#' y = c(5, 5, 5, 3, 3, 7)
#' 
#' # By default, the index value is based on order of occurrence
#' to_index(x)
#' to_index(y)
#' to_index(x, y)
#' 
#' # Use the order of the input values with sorted=TRUE
#' to_index(x, sorted = TRUE)
#' to_index(y, sorted = TRUE)
#' to_index(x, y, sorted = TRUE)
#' 
#' # To get the values to which the index refer, use items.out
#' to_index(x, items.out = TRUE)
#' 
#' # play around with the format of the output
#' to_index(x, items.out = TRUE, out.list = TRUE)
#' to_index(x, items.out = TRUE, out.list = TRUE, items.df = TRUE)
#' 
#' # multiple items are by default coerced into a single character string ...
#' to_index(x, y, items.out = TRUE)
#' 
#' # ... to avoid this, use items.df = TRUE
#' to_index(x, y, items.out = TRUE, items.df = TRUE)
#' to_index(x, y, items.out = TRUE, items.df = TRUE, sorted = TRUE)
#' 
#' # NAs considered as valid
#' x_NA = c("u", NA, "a", "a", "s", "u", "u")
#' to_index(x_NA, items.out = TRUE)
#' to_index(x_NA, items.out = TRUE, sorted = TRUE)
#' 
#' 
to_index = function(..., list = NULL, sorted = FALSE, items.out = FALSE, out.list = FALSE,
                    items.df = FALSE, items.join = "_", internal = FALSE){

  check_arg(sorted, items.out, out.list, items.df, "logical scalar")
  check_arg(items.join, "character scalar")
  
  check_arg(list, "NULL list")
  IS_DOT = TRUE
  if (!is.null(list)) {
    dots = list
    IS_DOT = FALSE
  } else {
    if(!internal){
      check_arg(..., "vector mbt")
    }
    dots = list(...)
  }  

  Q = length(dots)
  n_all = lengths(dots)
  n = n_all[1]

  if(length(unique(n_all)) != 1){
    stopi("All elements in `...` should be of the same length (current lenghts are {enum?n_all}).")
  }

  #
  # Creating the ID
  #
  
  info = to_index_internal(dots)
  index = info$index
  if (sorted || items.out) {
    
    # vector of the first items
    items_unik = vector("list", Q)
    for (q in 1:Q) {
      items_unik[[q]] = dots[[q]][info$first_obs]
    }
    
    if (sorted) {
      x_order = do.call(order, items_unik)
      index = order(x_order)[index]
      for (q in 1:Q) {
        items_unik[[q]] = items_unik[[q]][x_order]
      }
    }
    
    items = NULL
    if(items.df){
      # Putting into a DF => we take care of names
      user_names = names(dots)
      if(is.null(user_names)){
        user_names = character(Q)
      }
      
      if(IS_DOT){
        mc_dots = match.call(expand.dots = FALSE)[["..."]]
      }
      
      for(q in 1:Q){
        if(nchar(user_names[q]) == 0){
          is_done = FALSE
          if(IS_DOT){
            mcq = mc_dots[[q]]
            if(is.name(mcq)){
              user_names[q] = as.character(mcq)[1]
              is_done = TRUE
            } else if(is.call(mcq) && as.character(mcq[[1]])[1] == "$"){
              user_names[q] = as.character(mcq[[3]])[1]
              is_done = TRUE
            }
          }
          if(!is_done){
            user_names[q] = paste0("x", q)
          }          
        }
      }

      names(items_unik) = user_names

      items = as.data.frame(items_unik)
      row.names(items) = 1:nrow(items)
      
    } else {
      # we "paste" them if Q > 1
      if(Q == 1){
        items = items_unik
      } else {
        arg_list = items_unik
        arg_list$sep = items.join
        items = do.call("paste", arg_list)
      }
    }

    if(items.out){
      res = list(index = index, items = items)
    } else {
      res = index
    }   
    
  } else {
    res = index
  }

  if(items.out && isFALSE(out.list)){
    res_tmp = res$index
    attr(res_tmp, "items") = res$items
    res = res_tmp
  }

  res
}


to_index_internal = function(x){
  # x: a list of vectors
  # x must be a list
  
  if (!is.list(x)) {
    check_arg(x, "list")
  }
  
  return(cpp_to_index(x))
}




