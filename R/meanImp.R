#' A Function to impute means from all other non-NA cells in a column.
#' @param x A vector
#' @keywords impute mean
#' @export
#' @examples
#' data(iris)
#' meanImp(iris$Petal.Length)

meanImp <- function(x){
  meanX <- mean(x, na.rm = TRUE)
  x <- ifelse(is.na(x), meanX, x)
  return(x)
}
