#' Reduce Cardinality of Factor Variables to only keep Hi Frequency Levels
#'
#' @param x - vector of factor values
#' @param y - minimum number of occurrences to keep
#' @param na.handle - how to treat NA values, "NA", "Other" or FALSE. If FALSE, leave NA as is, if "Other", then convert NA to other as well. If "NA", convert NA to "NA".
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
hiFreqFact <- function(x, y = 1000, na.handle = 'Other'){
  x <- as.character(x)
  rt1 <- rankTab(x)
  rt1 <- rt1[rt1$Freq > y,]
  x <- {
    if (na.handle != FALSE){
      if (na.handle == 'NA'){
        ifelse(is.na(x), 'NA', ifelse(x %in% rt1$x, x, 'other'))
      } else{
        ifelse(x %in% rt1$x, x, 'other')
      }
    } else {
      ifelse(x %in% rt1$x | is.na(x), x, 'other')
    }
  }
  x <- as.factor(x)
  return(x)
}




