


####
#### user-level ####
####

indexthis_vendor = function(path_r = "R/to_index.R", path_cpp = "R/to_index.cpp"){
  path_r = system.file("inst/to_index.R", package = "indexthis")
  if(identical(path_r, "")){
    stop("Unexpected bug. The package code could not be located.")
  }
  dest_path_r = file.path(path, path_r)
  if(file.exists(dest_path_r)){
    old_r_code = clean_to_index_r_code(dest_r_path)
    if(!is_same_code(current_r_code, old_r_code)){
      message("Updating the file '", path_r, "'")
      writeLines(current_r_code, path_r)
    }
  }
  
  path_cpp = system.file("inst/to_index.cpp", package = "indexthis")
  if(identical(path_cpp, "")){
    stop("Unexpected bug. The package code could not be located.")
  }
  dest_path_cpp = file.path(path, path_cpp)
  if(file.exists(dest_path_cpp)){
    old_cpp_code = clean_to_index_cpp_code(dest_cpp_path)
    if(!is_same_code(current_cpp_code, old_cpp_code)){
      message("Updating the file '", path_cpp, "'")
      writeLines(current_cpp_code, path_cpp)
    }
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
  x = gsub("^(\\S)", "\n\\1", x)
  
  c(first_lines, x, "")
}


gen_vendor_code = function(){
  
  if(!is_indexthis_root()) return(NULL)
  
  current_r_code = clean_to_index_r_code()
  
  path_r = "inst/to_index.R"
  if(!file.exists(path_r)){
    writeLines(current_r_code, path_r)
  } else {
    old_r_code = readLines(path_r)
    if(!is_same_code(current_r_code, old_r_code)){
      message("Updating the R code in inst/")
      writeLines(current_r_code, path_r)
    }
  }
  
  current_cpp_code = clean_to_index_cpp_code()
  path_cpp = "inst/to_index.cpp"
  if(!file.exists(path_cpp)){
    writeLines(current_cpp_code, path_cpp)
  } else {
    old_cpp_code = readLines(path_cpp)
    if(!is_same_code(current_cpp_code, old_cpp_code)){
      message("Updating the cpp code in inst/")
      writeLines(current_cpp_code, path_cpp)
    }
  }
  
  
}




