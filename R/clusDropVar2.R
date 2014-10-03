#' A Function to loop thru and drop variables from a data.frame for clustering uses.
#'

#' @param x A data.frame object
#' @param y A numeric vector of cluster numbers to try.
#' @param threshold A number for 0 - 1 for silhouette width to excede.
#' @param vte A character vector containing column names to exclude from consideration.

#' @keywords names class
#' @export
#' @examples
#' clusDropVar(iris, 3:5, threshold = .6, vte = 'Species')

clusDropVar <- function(x, y, threshold = .5, vte = c() ){
	require(cluster)
	require(ade4)
#x <- iris
	varsToUse <- names(x)
	#if(is.null(vte)){
	#		vte <- c()
	#}
	varsToExclude <- c(vte)
	scoresFull <- list()
	minNumOfVars <- 2

	for(q in 1:length(varsToUse)){ # q = 1
	
	scores1 <- list()
	vtu1 <- varsToUse[!(varsToUse %in% varsToExclude)]
	if(length(vtu1) > 2){

	for(i in 1:length(vtu1)){  # i =1
		ddm1 <- dudi.mix(x[,vtu1[-i]], add.square = FALSE, scannf = F, nf = 20)

		for(j in y){ # j = 2

			set.seed(456)
			pam1 <- clara(ddm1$li[,1:min(5,ncol(ddm1$li))], k = j, samples = 50)
			sil1 <- pam1$silinfo$avg.width
			scores1[[i]] <- data.frame(varName = as.character(vtu1[i]), score = sil1)

			#if(i %% 10 == 0){

			print(paste('i: ', i, ' | k: ',j,' | sil: ',sil1, ' | var: ',vtu1[i], sep = ''));flush.console()
			#}
		}  		

	}

	scores1Df <- do.call(rbind.data.frame, scores1)
	scores1Df$varName <- as.character(scores1Df$varName)
	
	gc()

	varToDrop <- scores1Df[which(scores1Df$score == max(scores1Df$score)),'varName']
	print(paste('Dropping var: ', varToDrop, ' | Best Score : ', max(scores1Df$score) , sep = ''));flush.console()
	varsToExclude <- c(varsToExclude, varToDrop)
		if(max(scores1Df$score) >= threshold){
			vtu1 <- varsToUse[!(varsToUse %in% varsToExclude)]
			outPutList <- list()
			outPutList[['varsToUse']] <- vtu1 
			outPutList[['varsToExclude']] <- varsToExclude
			outPutList[['bestScore']] <- max(scores1Df$score)

			return(outPutList)
			stop()

		}
	}
}



}

