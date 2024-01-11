#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2024-01-11
# ~: main index function
#------------------------------------------------------------------------------#



to_index = function(..., sorted = FALSE, add_items = FALSE, items.list = FALSE,
                      multi.df = FALSE, multi.join = "_", internal = FALSE){

  if(!internal) check_arg(..., "vector mbt")
  check_arg(sorted, add_items, items.list, "logical scalar")
  check_arg(multi.join, "character scalar")

  dots = list(...)

  Q = length(dots)
  n_all = lengths(dots)
  n = n_all[1]

  if(length(unique(n_all)) != 1){
    stop("All elements in `...` should be of the same length (current lenghts are ", enumerate_items(n_all), ").")
  }

  #
  # Creating the ID
  #

  if(Q == 1){
    if(sorted && !is.numeric(dots[[1]]) && !is.character(dots[[1]])){
      # general way => works for any type with a sort method
      f = dots[[1]]
      res_raw = quickUnclassFactor(f, addItem = TRUE, sorted = FALSE)
      obs_1st = cpp_get_first_item(res_raw$x, length(res_raw$items))
      f_unik = f[obs_1st]
      f_order = order(f_unik)
      x_new = order(f_order)[res_raw$x]
      if(add_items){
        items_new = f_unik[f_order]
        res = list(x = x_new, items = items_new)
      } else {
        res = x_new
      }

    } else {
      res = quickUnclassFactor(dots[[1]], addItem = add_items, sorted = sorted)
    }

  } else {

    QUF_raw = list()
    for(q in 1:Q){
      QUF_raw[[q]] = quickUnclassFactor(dots[[q]], sorted = FALSE, addItem = TRUE)
    }

    # Then we combine
    power = floor(1 + log10(sapply(QUF_raw, function(x) length(x$items))))

    is_large = sum(power) > 14
    if(is_large){
      # 15 Aug 2021, finally found a solution. It was so obvious with hindsight...
      QUF_raw_value = lapply(QUF_raw, `[[`, 1)
      order_index = do.call(order, QUF_raw_value)
      index = cpp_combine_clusters(QUF_raw_value, order_index)
    } else {
      # quicker, but limited by the precision of doubles
      index = QUF_raw[[1]]$x
      for(q in 2:Q){
          index = index + QUF_raw[[q]]$x*10**sum(power[1:(q-1)])
      }
    }

    res = quickUnclassFactor(index, addItem = add_items || sorted, sorted = sorted)

    if(add_items || sorted){
      # we re order appropriately
      # f prefix means factor

      obs_1st = cpp_get_first_item(res$x, length(res$items))

      f_all = list()
      for(q in 1:Q){
          f_all[[q]] = dots[[q]][obs_1st]
      }

      f_order = do.call("order", f_all)

      x_new = order(f_order)[res$x]

      if(multi.df){
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
            my_names[q] = deparse_long(mc_dots[[q]])
          }
        }

        names(f_all) = my_names

        f_df = as.data.frame(f_all)
        items_new = f_df[f_order, , drop = FALSE]
        row.names(items_new) = 1:nrow(items_new)
      } else {
        # we "paste" them
        arg_list = f_all
        arg_list$sep = multi.join
        f_char = do.call("paste", arg_list)
        items_new = f_char[f_order]
      }

      if(add_items){
        res = list(x = x_new, items = items_new)
      } else {
        res = x_new
      }
    }
  }

  if(add_items && isFALSE(items.list)){
    res_tmp = res$x
    attr(res_tmp, "items") = res$items
    res = res_tmp
  }

  res
}


to_index_internal = function(x){
  # x: a list of vectors
  
  
  
}




