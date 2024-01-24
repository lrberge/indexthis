#----------------------------------------------#
# Author: Laurent Berge
# Date creation: Wed Jul 27 10:06:18 2022
# ~: checking functions
#----------------------------------------------#

# 0 dependencies checking script for basic types
# this is a bit like dreamerr's

####
#### checking ####
####


check_logical = function(x, null = FALSE, scalar = FALSE, l0 = FALSE, no_na = FALSE, mbt = FALSE, up = 0){
  set_up(up + 1)

  if(missing(x)){
    if(mbt){
      x_dp = deparse_short(substitute(x))
      stop_up("The argument `", x_dp, "` must be provided. PROBLEM: it is missing.")
    }

    return()
  }

  if(null && is.null(x)){
    return()
  }

  if(!is.logical(x)){
    x_dp = deparse_short(substitute(x))
    nullable = if(null) "(nullable) " else ""
    type = if(scalar) "scalar" else "vector"
    stop_up("The ", nullable, "argument `", x_dp, "` must be a logical ", type, ".",
            " PROBLEM: it is not logical, it is of class ", enum(class(x)), ".")
  }

  len_x = length(x)
  if(len_x == 0 &&  l0) return()

  if(scalar){
    if(missing(no_na)) no_na = TRUE

    if(len_x != 1){
      x_dp = deparse_short(substitute(x))
      nullable = if(null) "(nullable) " else ""
      stop_up("The ", nullable, "argument `", x_dp, "` must be a logical scalar.",
              " PROBLEM: it is not of length 1, it is of length ", length(x), ".")
    }
  }

  if(no_na && anyNA(x)){
    x_dp = deparse_short(substitute(x))
    nullable = if(null) "(nullable) " else ""
    type = if(scalar) "scalar" else "vector without NA"
    problem = if(scalar) "it is equal to NA" else "it contains NA values"
    stop_up("The ", nullable, "argument `", x_dp, "` must be a logical ", type, ". ",
            "PROBLEM: ", problem, ".")
  }

}

check_numeric = function(x, null = FALSE, scalar = FALSE, l0 = FALSE, no_na = FALSE, 
                         mbt = FALSE, integer = FALSE, complex = FALSE, up = 0){
  set_up(up + 1)
  
  if(missing(x)){
    if(mbt){
      x_dp = deparse_short(substitute(x))
      stop_up("The argument `", x_dp, "` must be provided. PROBLEM: it is missing.")
    }

    return()
  }

  if(null && is.null(x)){
    return()
  }

  if((!complex && !is.numeric(x)) || (complex && !is_num_complex(x))){
    x_dp = deparse_short(substitute(x))
    nullable = if(null) "(nullable) " else ""
    vector_type = if(scalar) "scalar" else "vector"
    type = if(integer) "an integer" else "a numeric"
    if(complex) paste0(type, ", or complex,")
    
    stop_up("The ", nullable, "argument `", x_dp, "` must be ", type, " ", vector_type, ".",
            " PROBLEM: it is not ", type, ", it is of class ", enum(class(x)), ".")
  }

  len_x = length(x)
  if(len_x == 0 &&  l0) return()

  if(scalar){
    if(missing(no_na)) no_na = TRUE

    if(len_x != 1){
      x_dp = deparse_short(substitute(x))
      nullable = if(null) "(nullable) " else ""
      type = if(integer) "an integer" else "a numeric"
      
      stop_up("The ", nullable, "argument `", x_dp, "` must be ", type, " scalar.",
              " PROBLEM: it is not of length 1, it is of length ", length(x), ".")
    }
  }

  if(no_na && anyNA(x)){
    x_dp = deparse_short(substitute(x))
    nullable = if(null) "(nullable) " else ""
    vector_type = if(scalar) "scalar" else "vector without NA"
    type = if(integer) "an integer" else "a numeric"
    problem = if(scalar) "it is equal to NA" else "it contains NA values"
    
    stop_up("The ", nullable, "argument `", x_dp, "` must be ", type, " ", vector_type, ". ",
            "PROBLEM: ", problem, ".")
  }
  
  if(integer){
    if(complex){
      for(part in 1:2){
        z = if(part == 1) Re(x) else Im(x)
        if(any(z != round(z))){
          x_dp = deparse_short(substitute(x))
          i = which(z != round(z))[1]
          intro = ""
          if(part == 1 && all(Im(x) == 0)){
            msg = paste0("is equal to ", z)
          } else {
            msg = paste0("has ", c(" a real", "an imaginary")[i], " part equal to ", z)
          }
          stop_up("The argument `", x_dp, "` must be an integer ", 
                  ifelse(scalar, "scalar", "vector"), ".",
                  "\nPROBLEM: the ", n_th(i), " value ", msg, ", not an integer.")
        }
      }
    } else {
      if(any(x != round(x))){
        x_dp = deparse_short(substitute(x))
        i = which(x != round(x))[1]
        stop_up("The argument `", x_dp, "` must be an integer ",
                ifelse(scalar, "scalar", "vector"), ".",
                "\nPROBLEM: the ", n_th(i), " value ",
                "is equal to ", x[i], ", not an integer.")
      }
    }    
  }

}

check_character = function(x, null = FALSE, scalar = FALSE, l0 = FALSE, 
                          no_na = FALSE, mbt = TRUE, up = 0){
                            
  set_up(up + 1)

  if(missing(x)){
    if(mbt){
      x_dp = deparse_short(substitute(x))
      stop_up("The argument `", x_dp, "` must be provided. PROBLEM: it is missing.")
    }

    return()
  }

  if(null && is.null(x)){
    return()
  }

  if(!is.character(x)){
    x_dp = deparse_short(substitute(x))
    nullable = if(null) "(nullable) " else ""
    type = if(scalar) "scalar" else "vector"
    stop_up("The ", nullable, "argument `", x_dp, "` must be a character ", type, ".",
            " PROBLEM: it is not character, it is of class ", enum(class(x)), ".")
  }

  len_x = length(x)
  if(len_x == 0 && l0 && !scalar) return()

  if(scalar){
    if(missing(no_na)) no_na = TRUE

    if(len_x != 1){
      x_dp = deparse_short(substitute(x))
      nullable = if(null) "(nullable) " else ""
      stop_up("The ", nullable, "argument `", x_dp, "` must be a character scalar.",
              " PROBLEM: it is not of length 1, it is of length ", length(x), ".")
    }
  }

  if(no_na && anyNA(x)){
    x_dp = deparse_short(substitute(x))
    nullable = if(null) "(nullable) " else ""
    type = if(scalar) "scalar" else "vector without NA"
    problem = if(scalar) "it is equal to NA" else "it contains NA values"
    stop_up("The ", nullable, "argument `", x_dp, "` must be a character ", type, ". ",
            "PROBLEM: ", problem, ".")
  }

}

check_set_character = function(x, null = FALSE, scalar = FALSE, l0 = FALSE, 
                               no_na = FALSE, mbt = TRUE){

  if(missing(x)){
    if(mbt){
      x_dp = deparse_short(substitute(x))
      stop_up("The argument `", x_dp, "` must be provided. PROBLEM: it is missing.")
    }

    return(NULL)
  }

  if(null && is.null(x)){
    return(NULL)
  }

  if(!is.atomic(x)){
    x_dp = deparse_short(substitute(x))
    stop_up("Argument `", x_dp, "` must be atomic. ",
            "\nPROBLEM: Currently it is of the non-atomic class ", class(x)[1], ".")
  }

  if(!is.character(x) || !identical(class(x), "character")){
    x = as.character(x)
  }

  len_x = length(x)
  if(len_x == 0 && l0 && !scalar){
    return(character(0))
  }

  if(scalar){
    if(missing(no_na)) no_na = TRUE

    if(len_x != 1){
      x_dp = deparse_short(substitute(x))
      nullable = if(null) "(nullable) " else ""
      stop_up("The ", nullable, "argument `", x_dp, "` must be a character scalar.\n",
              " PROBLEM: it is not of length 1, it is of length ", length(x), ".")
    }
  }

  if(no_na && anyNA(x)){
    x_dp = deparse_short(substitute(x))
    nullable = if(null) "(nullable) " else ""
    type = if(scalar) "scalar" else "vector without NA"
    problem = if(scalar) "it is equal to NA" else "it contains NA values"
    stop_up("The ", nullable, "argument `", x_dp, "` must be a character ", type, ". \n",
            " PROBLEM: ", problem, ".")
  }
  
  return(x)
}


check_envir = function(x){

  if(!inherits(x, "environment")){
    x_dp = deparse_short(substitute(x))
    stop_up("The argument `", x_dp, "` must be an environment (ex: parent.frame()). ",
            "PROBLEM: it is not an environment, it is of class ", enum(class(x)), ".")
  }

}

check_function = function(x, null = FALSE, up = 0, argname = NULL){
  set_up(up + 1)
  
  if(null && is.null(x)){
    return(NULL)
  }
  
  if(!is.function(x)){
    if(is.null(argname)){
      x_dp = deparse_short(substitute(x))
    }
    
    stop_up("The argument `", x_dp, "` must be a function. ",
            "PROBLEM: it is not a function, it is of class ", enum(class(x)), ".")
  }
}


check_set_dots = function(..., mc = NULL, mbt = FALSE, character = FALSE,
                          no_na = FALSE, scalar = FALSE, nofun = FALSE){
  # check the dots arguments

  n = ...length()
  if(n == 0){

    if(mbt){
      stop_up("At least one element in `...` must be provided. PROBLEM: `...` is empty.")
    } else {
      return(list())
    }

  }

  if(is.null(mc)){
    sysOrigin = sys.parent()
    mc = match.call(definition = sys.function(sysOrigin),
                    call = sys.call(sysOrigin), expand.dots = FALSE)
  }

  dots = vector("list", n)
  dots_nm = ...names()

  # We first catch evaluation problems
  for(i in 1:n){
    elem = try(...elt(i), silent = TRUE)

    if(isError(elem) || (nofun && is.function(elem))){
      nm = if(is.null(dots_nm)) "" else dots_nm[i]
      if(is.na(nm)) nm = ""

      mc_dots = mc[["..."]]
      value = deparse_short(mc_dots[[i]])

      nm = ifelse(nchar(nm) == 0, 
                  paste0(" (", value, ")"),
                  paste0(" (", nm, " = ", value))
      
      if(isError(elem)){
        if(grepl("try(...", elem, fixed = TRUE)){
          elem = gsub("^[^:]+:", "", elem)
        }

        stop_up("In the argument `...`, the ", n_th(i), " element", nm,
                " raises an error:\n", elem)
      } else {
        stop_up("In the argument `...`, the elements must not be functions.",
                "\nPROBLEM: the ", n_th(i), " element", nm, " is a function.")
      }
    }

    dots[[i]] = elem
  }
  
  # now we catch type problems
  if(scalar){
    if(any(lengths(dots) != 1)){
      len_dots = lengths(dots)
      i_pblm = which(len_dots != 1)
      len_pblm = len_dots[i_pblm]

      # I purposefully copy-paste in each block
      # otherwise the code becomes too complicated and especially more difficult to debug

      # We try to give as much info as possible
      n_pblm = length(i_pblm)
      nm_pblm = character(n_pblm)

      if(!is.null(dots_nm)){
        rep = dots_nm[i_pblm]
        rep[is.na(rep)] = ""
        nm_pblm = rep
      }

      # the value of the variables
      mc_dots = mc[["..."]]
      value_all = sapply(i_pblm, function(i) deparse_short(mc_dots[[i]]))

      stop_up("In the argument `...`, all elements must be scalars (i.e. of length 1).",
              "\nPROBLEM: The ", n_th(i_pblm), " element(s) are not of length 1.")
    }
  }

  if(character){
    # we convert to character => requirement is atomicity
    if(!all(sapply(dots, is.atomic))){      
      i_pblm = which(!sapply(dots, is.atomic))

      # We try to give as much info as possible
      n_pblm = length(i_pblm)
      nm_pblm = character(n_pblm)

      if(!is.null(dots_nm)){
        rep = dots_nm[i_pblm]
        rep[is.na(rep)] = ""
        nm_pblm = rep
      }

      # the value of the variables
      mc_dots = mc[["..."]]
      value_all = sapply(i_pblm, function(i) deparse_short(mc_dots[[i]]))

      stop_up("In the argument `...`, all elements must be atomic (i.e. convertible to a character string).",
              "\nPROBLEM: The ", n_th(i_pblm), " element(s) cannot be converted.")
    }

    true_character = function(x) is.character(x) && identical(class(x), "character")
    i_no_char = which(!sapply(dots, true_character))
    for(i in i_no_char){
      dots[[i]] = as.character(dots[[i]])
    }
  }

  if(no_na){
    if(any(sapply(dots, anyNA))){
      i_pblm = which(sapply(dots, anyNA))

      # We try to give as much info as possible
      n_pblm = length(i_pblm)
      nm_pblm = character(n_pblm)

      if(!is.null(dots_nm)){
        rep = dots_nm[i_pblm]
        rep[is.na(rep)] = ""
        nm_pblm = rep
      }

      # the value of the variables
      mc_dots = mc[["..."]]
      value_all = sapply(i_pblm, function(i) deparse_short(mc_dots[[i]]))

      stop_up("In the argument `...`, all elements must be without NA.\nPROBLEM: ",
              "The ", enum(n_th(i_pblm)), " element(s) contain(s) NA values.")
    }
  }

  names(dots) = dots_nm

  return(dots)
}

####
#### utilities ####
####

deparse_long = function(x){
  x_dp = deparse(x, width.cutoff = 500L)
  if(length(x_dp) > 1){
    x_dp = paste0(x_dp, collapse = "\n")
  }

  x_dp
}

n_th = function(x, letters = TRUE, compact = FALSE){
  # The main purpose of this function is for smallish 'x'
  # only to print info, not performance oriented.

  if(is.character(x)) return(x)

  is_compact = FALSE
  if(length(x) > 1 && all(diff(x) == 1)){
    is_compact = TRUE
    x = x[c(1, length(x))]
  }

  res = character(length(x))

  if(letters){
    # We don't go all the way, it makes no sense

    res[x == 0] = "zeroth"

    dict = c("first", "second", "third", "fourth", "fifth", "sixth", "seventh",
             "eighth", "nineth", "tenth", "eleventh", "twelfth", "thirteenth",
             "fourteenth", "fifteenth", "sixteenth", "seventeenth", "eighteenth",
             "nineteenth", "twentyth")

    qui = x >= 1 & x <= 20
    res[qui] = dict[x[qui]]

    if(!any(res == "")){
      if(is_compact){
        res = paste0(res[1], " to ", res[2])
      }
      return(res)
    }
  }

  qui = res == ""
  rest = x[qui] %% 10
  rest[rest == 0 | rest >= 4] = 4
  rest[x[qui] %in% 11:13] = 4
  postfix = c("st", "nd", "rd", "th")

  res[qui] = paste0(x[qui], postfix[rest])

  if(is_compact){
    res = paste0(res[1], " to ", res[2])
  }

  res
}

enum = function(x){
  if(length(x) < 2){
    return(paste0(x, collapse = " and "))
  } else {
    res = paste0(x[-length(x)], collapse = ", ")
    res = paste0(res, " and ", x[length(x)])
    return(res)
  }
}


####
#### dreamerr's copies ####
####


# in this version of stop_up, there is no interpolation because
# we don't want the stringmagic dependency
# 
stop_up = function(..., up = 1, msg = NULL, envir = parent.frame()){

  main_msg = paste0(...)

  # up with set_up
  mc = match.call()
  
  up = up + 1

  sc = sys.calls()
  
  # only the original call
  my_call = sys.call(sys.parent(up))
  my_call = deparse(my_call)[1]
  nmax = 50
  if(nchar(my_call) > nmax) my_call = paste0(substr(my_call, 1, nmax - 1), "...")

  intro = paste0("in ", my_call)

  main_msg = fit_screen(main_msg)

  if(!is.null(msg)){
    if(length(msg) > 1){
      msg = paste(msg, collapse = "")
    }
    msg = fit_screen(msg)
    on.exit(message(msg))
  }

  stop(intro, ": \n", main_msg, call. = FALSE)

}

warn_up = function (..., up = 1, immediate. = FALSE, envir = parent.frame()){

  message = paste0(...)
  
  mc = match.call()
  
  up = up + 1
  
  my_call = sys.call(sys.parent(up))
  my_call = deparse(my_call)[1]
  
  nmax = 50
  if (nchar(my_call) > nmax){
    my_call = paste0(substr(my_call, 1, nmax - 1), "...")
  }
      
  warning("In ", my_call, ":\n ", fit_screen(message),
          call. = FALSE, immediate. = immediate.)
}


fit_screen = function(msg, width = NULL, leading_ws = TRUE, leader = ""){
  # makes a message fit the current screen, by cutting the text at the appropriate location
  # msg must be a character string of length 1
  
  if(length(msg) == 0) return(msg)

  # Note that \t are NOT handled
  
  # eval
  width = check_set_width(substitute(width))

  N_LEAD = nchar(leader)

  if(width > 1){
    MAX_WIDTH = width
  } else {
    MAX_WIDTH = getOption("width") * width
  }

  MAX_WIDTH = max(MAX_WIDTH, 15)

  res = c()

  msg_split = strsplit(msg, "\n", fixed = TRUE)[[1]]

  for(m in msg_split){
    if(nchar(m) <= MAX_WIDTH){
      res = c(res, paste0(leader, m))
    } else {
      # we apply a splitting algorithm

      lead_ws = gsub("^([ \t]*).*", "\\1", m, perl = TRUE)
      m = trimws(m)
      N_LEAD_WS = nchar(lead_ws)
      add_lead = TRUE
      first = TRUE

      m_split = strsplit(m, "(?<=[^ ]) ", perl = TRUE)[[1]]

      while(TRUE){

        if(add_lead){
          width = MAX_WIDTH - N_LEAD_WS - N_LEAD
          prefix = paste0(leader, lead_ws)
        } else {
          width = MAX_WIDTH - N_LEAD
          prefix = leader
        }

        if(sum(nchar(m_split) + 1) - 1 <= width){
          res = c(res, paste0(prefix, paste(m_split, collapse = " ")))
          break
        }

        where2split = which.max(cumsum(nchar(m_split) + 1) - 1 > width) - 1
        res = c(res, paste0(prefix, paste(m_split[1:where2split], collapse = " ")))
        m_split = m_split[-(1:where2split)]

        if(!leading_ws && first){
          add_lead = FALSE
          first = FALSE
        }

      }
    }
  }

  paste(res, collapse = "\n")
}

isError = function(x){
  inherits(x, "try-error")
}


