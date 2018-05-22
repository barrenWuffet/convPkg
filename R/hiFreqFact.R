#' Reduce Cardinality of Factor Variables to only keep Hi Frequency Levels
#'
#' @param x - vector of factor values
#' @param y - minimum number of occurrences to keep
#'
#' @return
#' @export
#'
#' @examples
hiFreqFact <- function(x, y = 1000){
  x <- as.character(x)
  rt1 <- rankTab(x)
  rt1 <- rt1[rt1$Freq > y,]
  x <- ifelse(x %in% rt1$x, x, 'other')
  x <- as.factor(x)
  return(x)
}
