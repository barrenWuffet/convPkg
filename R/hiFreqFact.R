#' Reduce Cardinality of Factor Variables to only keep Hi Frequency Levels
#'
#' @param x - vector of factor values
#' @param y - minimum number of occurrences to keep
#'
#' @return a factor vector with all levels occurring less than the minimum # of times converted to 'other'
#' @export
#'
#' @examples
#' v1 <- c(rep('a',25), rep('b',50), rep('c',3))
#' df1 <- data.frame(col1 = v1)
#' df1$col2 <- hiFreqFact(df1$col1, y = 10)
#' summary(df1)
#'
hiFreqFact <- function(x, y = 1000){
  x <- as.character(x)
  rt1 <- rankTab(x)
  rt1 <- rt1[rt1$Freq > y,]
  x <- ifelse(x %in% rt1$x, x, 'other')
  x <- as.factor(x)
  return(x)
}




