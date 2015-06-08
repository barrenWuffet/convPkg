#' A Function to print the column number, name and variable type for each column in a data.frame.
#'
#' This function prints the column number, name, and variable type for each column in a data.frame.
#' @param dataset A data.frame object
#' @keywords names class
#' @export
#' @examples
#' data(iris)
#' cnn(iris)

cnn  <- function(dataset){
	for(i in 1:length(dataset)){
	print(paste(i,": ", colnames(dataset)[i], " | ", class(dataset[,i]), sep=""))
	}
}
