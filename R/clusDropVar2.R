#' A Function to loop thru and drop variables from a data.frame for clustering uses.
#'

#' @param x A data.frame object
#' @param y A numeric vector of cluster numbers to try.
#' @param threshold A number for 0 - 1 for silhouette width to excede.
#' @param vte A character vector containing column names to exclude from consideration.
#' @param xLoopsBeforePrint A number for how many loops to skip before printing information to console. Higher number means less output.

#' @keywords names class
#' @importFrom cluster clara
#' @importFrom ade4 dudi.mix
#' @export
#' @examples
#' clusDropVar(iris, 3:5, threshold = .6, vte = "Species")

clusDropVar <- function(
  x,
  y,
  threshold = .5,
  vte = c() ,
  xLoopsBeforePrint = 10
){

  #x <- iris
  varsToUse <- names(x)
  #if(is.null(vte)){
  #    vte <- c()
  #}
  varsToExclude <- c(vte)

  for(q in 1:length(varsToUse)){
    # q = 1

    scores1 <- list()
    vtu1 <- varsToUse[!(varsToUse %in% varsToExclude)]
    if(length(vtu1) > 2){

      for(i in 1:length(vtu1)){
        # i =1
        ddm1 <- dudi.mix(x[,vtu1[-i]], add.square = FALSE, scannf = F, nf = 20)

        subList <- list()
        # Different #s of clusters
        for(j in y){
          # j = 2

          set.seed(456)
          pam1 <- clara(ddm1$li[,1:min(5,ncol(ddm1$li))], k = j, samples = 50)
          sil1 <- pam1$silinfo$avg.width
          subList[[j]] <- data.frame(varName = as.character(vtu1[i]), k = j, score = sil1)

          if(i %% xLoopsBeforePrint == 0){
            print(paste("i: ", i, " | k: ",j," | sil: ",sil1, " | var: ",vtu1[i], sep = ""));flush.console()
            gc()
          }
        }
        scores1[[i]] <- do.call(rbind.data.frame, subList)

      }

      scores1Df <- do.call(rbind.data.frame, scores1)
      scores1Df$varName <- as.character(scores1Df$varName)
      #print(scores1Df);flush.console()
      #gc()

      varToDrop <- scores1Df[which(scores1Df$score == max(scores1Df$score)), "varName"]
      print(paste("Dropping var: ", varToDrop, " | Best Score : ", max(scores1Df$score), " | vtu1 length: ",length(vtu1) , sep = ""));flush.console()
      varsToExclude <- c(varsToExclude, varToDrop)
      if(max(scores1Df$score) >= threshold){
        vtu1 <- varsToUse[!(varsToUse %in% varsToExclude)]
        outPutList <- list()
        outPutList[["varsToUse"]] <- vtu1
        outPutList[["varsToExclude"]] <- varsToExclude
        outPutList[["bestScore"]] <- max(scores1Df$score)

        return(outPutList)
        stop()

      }
    }
  }
}
