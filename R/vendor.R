


####
#### user-level ####
####



#' Vendor the `to_index` function
#' 
#' Utility to integrate the `to_index` function within a package without a dependency.
#' 
#' @param pkg Character scalar, default is `"."`. Location of the package directory where the files will be created. 
#' 
#' @details 
#' This is a utility to populate a package with the necessary code to run the `to_index` function. This avoids to create a dependency with the `indexthis` package.
#' 
#' The underlying code of `to_index` is in C++. Hence if the routines are to be included in a package, it needs to be registered appropriately. There are four cases: three are automatic, one requires a bit of copy pasting from the user. Let's review them.
#' 
#' It the target package already has C++ code and uses `Rcpp` or `cpp11` to make the linking, the function `indexthis_vendor` registers the main function as a `Rcpp` or `cpp11` routine, and all should be well.
#' 
#' If the target package has no C/C++ code at all, `indexthis_vendor` updates the NAMESPACE and registers the routine, and all should be well.
#' 
#' If the target package already has C/C++ code, this is more coplicated because there should be only one `R_init_pkgname` symbol and it should be existing already (see Writing R extensions, section "dyn.load and dyn.unload").
#' In that case, in the file `to_index.cpp` the necessary code to register the routine will be at the end of the file, within comments.
#' The (knowledgeable) user has to copy paste in the appropriate location, where she registers the existing routines.
#' 
#' 
#' @return 
#' This function does not return anything. Instead it writes two files: one in R (by default in the folder `./R`) and one in cpp (by default in the folder `src/`). Those files contain the necessary source code to run the function [`to_index`].
#' 
#' @examples 
#' 
#' ## DO NOT RUN: otherwise it will write in your packge workspace
#' # indexthis_vendor()
#' 
indexthis_vendor = function(pkg = "."){
  
  # we check if this is a package
  desc_path = file.path(pkg, "DESCRIPTION")
  if(!file.exists(desc_path)){
    stop("The function `indexthis_vendor` only works within packages (the argument `pkg` should point to one).",
         "\nPROBLEM: no DESCRIPTION file was found at the location",
         "\n'", normalizePath(pkg), "'")
  }
  
  desc = readLines(desc_path)
  pkg_name = trimws(gsub("^Package:", "", desc[1]))
  
  is_rcpp = any(grepl("^LinkingTo:.+Rcpp\\b", desc))
  message("Rcpp = ", is_rcpp)
  
  dest_path_r = normalizePath(file.path(pkg, "R", "to_index.R"))
  dest_path_cpp = normalizePath(file.path(pkg, "src", "to_index.cpp"))
  
  if(grepl("indexthis", normalizePath(pkg))){
    stop("Don't run this function in the indexthis package you fool!")
  }
  
  pkg_name_ = paste0("_", pkg_name)
  
  #
  # R
  #
  
  path_r = system.file("vendor/to_index.R", package = "indexthis")
  if(identical(path_r, "")){
    stop("Unexpected bug. The package code could not be located.")
  }
  
  current_r_code = readLines(path_r)
  current_r_code = gsub("_indexthis", pkg_name_, current_r_code)
  if(is_rcpp){
    current_r_code = gsub("\\.Call.+, ?", "cpp_to_index(", current_r_code)
  }
  
  if(file.exists(dest_path_r)){
    old_r_code = readLines(dest_path_r)
    
    if(!is_same_code(current_r_code, old_r_code)){
      message("Updating the file '", dest_path_r, "'")
      writeLines(current_r_code, dest_path_r)
    }
  } else {
    message("Creating the file '", dest_path_r, "'")
    create_dir(dest_path_r)
    writeLines(current_r_code, dest_path_r)
  }
  
  #
  # cpp
  #
  
  
  path_cpp = system.file("vendor/to_index.cpp", package = "indexthis")
  if(identical(path_cpp, "")){
    stop("Unexpected bug. The package code could not be located.")
  }
  
  current_cpp_code = readLines(path_cpp)
  current_cpp_code = gsub("_indexthis", pkg_name_, current_cpp_code)
  if(is_rcpp){
    i = which(grepl("^extern ", current_cpp_code))[1]
    current_cpp_code = c(current_cpp_code[1:(i - 1)], RCPP_EXPORT, "")
  }
  
  if(file.exists(dest_path_cpp)){
    old_cpp_code = readLines(dest_path_cpp)
    
    if(!is_same_code(current_cpp_code, old_cpp_code)){
      message("Updating the file '", dest_path_cpp, "'")
      writeLines(current_cpp_code, dest_path_cpp)
    }
  } else {
    message("Creating the file '", dest_path_cpp, "'")
    create_dir(dest_path_cpp)
    writeLines(current_cpp_code, dest_path_cpp)
  }
  
  #
  # namespace
  #
  
  namespace_path = file.path(pkg, "NAMESPACE")
  dynlib = paste0("useDynLib(", pkg_name, ", .registration = TRUE)")
  
  if(!file.exists(namespace_path)){
    message("Creating a NAMESPACE file with the associated `dynlib`")
    writeLines(c(dynlib, ""), namespace_path)
  } else {
    namespace = readLines(namespace_path)
    
    if(!any(grepl("useDynLib", namespace))){
      message("Updating the NAMESPACE file with the associated `dynlib`")
      new_namespace = c(dynlib, "", namespace)
      writeLines(new_namespace, namespace_path)
    } 
  }
  
  invisible()
}



####
#### internal ####
####

create_dir = function(path){
  dir = dirname(normalizePath(path))
  if(!dir.exists(dir)){
    dir.create(dir, recursive = TRUE)
  }
}

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

RCPP_EXPORT = c("// [[Rcpp::export(rng = false)]]",
                "SEXP cpp_to_index(SEXP x){",
                "  return indexthis::cpp_to_index_main(x);",
                "}")




