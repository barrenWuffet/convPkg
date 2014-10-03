#' A Function to rank a table() output from most frequently occuring to least.
#'

#' @param x A vector
#' @keywords table
#' @export
#' @examples
#' rankTab(iris$Species)


rankTab <- function(x){
tab1 <- data.frame(table(x))
tab1 <- tab1[order(-tab1$Freq),]
tab1
}