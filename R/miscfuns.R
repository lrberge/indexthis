


####
#### user-level ####
####



#' Vendor the `to_index` function
#' 
#' Utility to integrate the `to_index` function within a package without a dependency.
#' 
#' @param path_r Character scalar, default is `"R/to_index.R"`. Where to place the R-side of the `to_index` function. 
#' @param path_cpp Character scalar, default is `"src/to_index.cpp"`. Where to place the cpp-side of the `to_index` function. 
#' @param pkg Character scalar, default is `"."`. Location of the directory where the files will be created. 
#' 
#' @details 
#' This is a utility to populate a package with the necessary code to run the `to_index` function. This avoids to create a dependency with the `indexthis` package.
#' 
#' @return 
#' This function does not return anything. Instead it writes two files: one in R (by default in the folder `./R`) and one in cpp (by default in the folder `src/`). Those files contain the necessary source code to run the function [`to_index`].
#' 
#' @examples 
#' 
#' ## DO NOT RUN: otherwise it will write in your workspace
#' # indexthis_vendor()
#' 
indexthis_vendor = function(path_r = "R/to_index.R", path_cpp = "src/to_index.cpp", pkg = "."){
  
  dest_path_r = normalizePath(file.path(pkg, path_r))
  dest_path_cpp = normalizePath(file.path(pkg, path_cpp))
  
  if(grepl("indexthis", normalizePath(pkg))){
    stop("Don't run this function in the indexthis package you fool!")
  }
  
  #
  # R
  #
  
  path_r = system.file("vendor/to_index.R", package = "indexthis")
  if(identical(path_r, "")){
    stop("Unexpected bug. The package code could not be located.")
  }
  
  if(file.exists(dest_path_r)){
    current_r_code = readLines(path_r)
    old_r_code = clean_to_index_r_code(dest_path_r)
    
    if(!is_same_code(current_r_code, old_r_code)){
      message("Updating the file '", dest_path_r, "'")
      file.copy(path_r, dest_path_r, recursive = dir.exists(dest_path_r), 
                overwrite = TRUE)
    }
  } else {
    file.copy(path_r, dest_path_r, recursive = dir.exists(dest_path_r), 
              overwrite = TRUE)
  }
  
  #
  # cpp
  #
  
  
  path_cpp = system.file("vendor/to_index.cpp", package = "indexthis")
  if(identical(path_cpp, "")){
    stop("Unexpected bug. The package code could not be located.")
  }
  
  if(file.exists(dest_path_cpp)){
    current_cpp_code = readLines(path_cpp)
    old_cpp_code = clean_to_index_cpp_code(dest_path_cpp)
    
    if(!is_same_code(current_cpp_code, old_cpp_code)){
      message("Updating the file '", dest_path_cpp, "'")
      file.copy(path_cpp, dest_path_cpp, recursive = dir.exists(dest_path_cpp), 
                overwrite = TRUE)
    }
  } else {
    file.copy(path_cpp, dest_path_cpp, recursive = dir.exists(dest_path_cpp), 
              overwrite = TRUE)
  }
}



####
#### internal ####
####

renvir_get = function(key){
  # Get the values of envir variables
  # we also evaluate them

  value_raw = Sys.getenv(key)

  if(value_raw == ""){
      return(NULL)
  }

  # Any default value should be able to be evaluated "as such"
  value_clean = gsub("__%%;;", "\n", value_raw)
  value_clean = gsub("&quot;", '"', value_clean)
  value_clean = gsub("&apos;", "'", value_clean)

  value = eval(str2lang(value_clean))

  return(value)
}

is_indexthis_root = function(){
  isTRUE(renvir_get("indexthis_ROOT"))
}

indexthis_version = function(){
  desc = readLines("DESCRIPTION")
  version = grep("^Version", desc, value = TRUE)
  trimws(gsub("^[^ ]+ ", "", version))
}

is_same_code = function(x, y){
  length(x) == length(y) && all(x == y)
}

clean_to_index_r_code = function(path = "./R/to_index.R"){
  x = readLines(path)
  
  i_start = which(grepl("^to_index =", x))
  
  x = x[-(1:(i_start - 1))]
  
  first_lines = c("# ",
                  "# Generated automatically with indexthis::indexthis_vendor",
                  paste0("# this is indexthis version ", indexthis_version()),
                  "# ", "", "")
  
  x = x[!grepl("^\\s*#", x)]
  x = x[grepl("\\S", x)]
  
  c(first_lines, x, "")
}

clean_to_index_cpp_code = function(path = "./src/to_index.cpp"){
  x = readLines(path)
  
  i_start = which(grepl("^#include", x))[1]
  
  x = x[-(1:(i_start - 1))]
  
  first_lines = c("// ",
                  "// Generated automatically with indexthis::indexthis_vendor",
                  paste0("// this is indexthis version ", indexthis_version()),
                  "// ", "", "")
  
  x = x[!grepl("^\\s*//", x)]
  x = x[grepl("\\S", x)]
  
  c(first_lines, x, "")
}


gen_vendor_code = function(){
  
  if(!is_indexthis_root()) return(NULL)
  
  current_r_code = clean_to_index_r_code()
  
  path_r = "inst/vendor/to_index.R"
  if(!file.exists(path_r)){
    writeLines(current_r_code, path_r)
  } else {
    old_r_code = readLines(path_r)
    if(!is_same_code(current_r_code, old_r_code)){
      message("Updating the R code in inst/R")
      writeLines(current_r_code, path_r)
    }
  }
  
  current_cpp_code = clean_to_index_cpp_code()
  path_cpp = "inst/vendor/to_index.cpp"
  if(!file.exists(path_cpp)){
    writeLines(current_cpp_code, path_cpp)
  } else {
    old_cpp_code = readLines(path_cpp)
    if(!is_same_code(current_cpp_code, old_cpp_code)){
      message("Updating the cpp code in inst/cpp")
      writeLines(current_cpp_code, path_cpp)
    }
  }
  
  
}




