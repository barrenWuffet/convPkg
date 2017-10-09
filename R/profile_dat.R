profile_dat <- function(x){
	x_names <- names(x)
	for(i in x_names){
		# print(i);flush.console()
		vec1 <- as.character(x[,i])
		rt1 <- rankTab(vec1)
		out1 <- paste0(rt1[1:5,'x'], collapse = ' - ')
		out2 <- sum(is.na(vec1))
		out3 <- length(vec1[vec1 == ''])
		out4 <- paste('Var: ', i, ' | Values: ', out1, ' | NA_count: ', out2, ' | Blanks: ', out3, sep = '')
		print(out4);flush.console()
		# return(out4)
	}
}
