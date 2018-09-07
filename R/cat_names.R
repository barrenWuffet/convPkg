#' A function to print variable names for easy pasting to a new character vector.


cat_names <- function(x){
  cat(paste(",'", names(x), "'", "\n", sep = ""))
}
