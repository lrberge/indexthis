#------------------------------------------------------------------------------#
# Author: Laurent R. Bergé
# Created: 2024-01-17
# ~: Main tests
#------------------------------------------------------------------------------#



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
  dbl = round(rnorm(n, sd = 8), 1),
  dbl_int = round(rnorm(n, sd = 8)),
  char = sample(words, n, TRUE),
  date = sample(date_sample, n, TRUE),
  complex = complex(real = round(rnorm(n, sd = 4)), imaginary = round(rnorm(n, sd = 4)))
)


# single vector
for(i_type in seq_along(base)){
  cat(names(base)[i_type])
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















