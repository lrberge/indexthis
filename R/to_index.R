#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2024-01-11
# ~: main index function
#------------------------------------------------------------------------------#



to_index = function(..., list = NULL, sorted = FALSE, add_items = FALSE, out.list = FALSE,
                    items.df = FALSE, items.join = "_", internal = FALSE){

  check_arg(sorted, add_items, out.list, items.df, "logical scalar")
  check_arg(items.join, "character scalar")
  
  check_arg(list, "NULL list")
  if (!is.null(list)) {
    dots = list
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
  if (sorted || add_items) {
    
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
      mc_dots = match.call(expand.dots = FALSE)[["..."]]
      n_dots = length(mc_dots)
      mc_dots_names = names(mc_dots)
      if(is.null(mc_dots_names)) mc_dots_names = character(n_dots)

      my_names = character(n_dots)
      for(q in 1:n_dots){
        if(nchar(mc_dots_names[q]) > 0){
          my_names[q] = mc_dots_names[q]
        } else {
          my_names[q] = deparse(mc_dots[[q]])[1]
        }
      }

      names(items_unik) = my_names

      items = as.data.frame(items_unik)
      row.names(items) = 1:nrow(items)
      
    } else {
      # we "paste" them
      arg_list = items_unik
      arg_list$sep = items.join
      items = do.call("paste", arg_list)
    }

    if(add_items){
      res = list(index = index, items = items)
    } else {
      res = index
    }   
    
  } else {
    res = index
  }

  if(add_items && isFALSE(out.list)){
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
  
  for (i in seq_along(x)) {
    xi = x[[i]]
    
    if (!is.atomic(xi) || !is.null(dim(xi))) {
      stopi("The {nth?i} element is not a vector, instead it is of class {enum?class(x[[i]])}.")
    }
    
    if (!is.numeric(xi) && !is.character(xi) && !is.factor(xi)) {
      x[[i]] = as.character(xi)
    }
  }
  
  return(cpp_to_index(x))
}




