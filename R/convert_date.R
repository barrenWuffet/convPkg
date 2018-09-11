#' Converts all columns of class POSIXct or POSIXt in a data.frame to Date class.
#'
#' @param xx A data.frame containing columns of class POSIXct or POSIXt
#'
#' @return data.frame with any columns of class POSIXct or POSIXt converted to Dates
#' @export
#'
#' @examples df2 <- convert_date(df1)
#'
#'
#' @author \itemize{
#'    \item Andrei Rukavina - \url{https://github.com/arukavina}
#'    \item Thijn van der Heijden - \email{avanderheijden@@alixpartners.com}
#'    \item Zach Armentrout - \email{zarmentrout@@alixpartners.com}
#'    \item Qianbo Wang - \email{qwang@@alixpartners.com}
#'    \item James Wang - \email{swang@@alixpartners.com}
#' }
#'
#'
#'
convert_date <- function(xx){

  dateind <- names(which(sapply(sapply(xx, class),function(x) any(x %in% c("POSIXct", "POSIXt" )))))
  cat('found ',length(dateind), ' dates : \n' )
  lapply(dateind,function(x) cat(x,' --- \n'))

  xx[,dateind] <- data.frame(lapply(dateind,function(x) as.Date(xx[,x])))

  return(xx)
}
