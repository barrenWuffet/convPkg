#' A function to print variable names for easy pasting to a new character vector.
#'
#' @param x a data.frame
#' @export
#'
#' @examples cat_names(iris)

cat_names <- function(x){
  cat(paste(",'", names(x), "'", "\n", sep = ""))
}
