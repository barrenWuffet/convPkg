
#' Set the variable types for a data.frame
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' setTypes(iris)
setTypes <- function(x){ # x = dat1
  v1 <- names(x)
  l1 <- list()
  for(i in v1){  # i = v1[1]
    df1 <- data.frame(var = i, type = '', stringsAsFactors = F)
    print(i);flush.console()
    print(as.character(x[1:5,i]));flush.console()
    # t1 <-
    df1[1,'type'] <- readline(' Type - f / n / c?')
    l1[[i]] <- df1
  }

  df2 <- do.call(rbind, l1)

  charVar <- df2[df2$type == 'c','var']
  numVar <- df2[df2$type == 'n','var']
  factVar <- df2[df2$type == 'f','var']

  x[,charVar] <- as.data.frame(lapply(x[,charVar], as.character))
  x[,numVar] <- as.data.frame(lapply(x[,numVar], function(x) return(as.numeric(as.character(x)))))
  x[,factVar] <- as.data.frame(lapply(x[,factVar], as.factor))
  return(x)
}
