#' Function to write the variable importance plots of a gbm object to a pdf.
#'
#' @param gbm_object An object containing a GBM model and components
#' @param file_path A character string pointing to the directory of where to write the PDF containing the partial dependency plots.
#' @param file_name A character string pointing to the file of where to write the PDF containing the partial dependency plots.
#'
#' @return NA - writes to file.
#' @export
#' @author Arshan Tarapore \email{aktarapore@gmail.com}
#'
#' @examples
#' exportGBMtoPDF(gbm1, '/data/output/', 'output.pdf')
#'
#'
exportGBMtoPDF <- function(gbm_object,file_path,file_name){
  if(class(gbm_object) != "gbm")
    stop("gbm_object parameter is not of class'gbm'")
  if(class(file_path) != "character")
    stop("file_path parameter is not of class 'character'")
  if(class(file_name) != "character")
    stop("file_name parameter is not of class 'character'")
  sgbm1 <-summary(gbm_object)
  if(substr(file_name,nchar(file_name)-4+1,nchar(file_name)) == ".pdf") {
    pdf(paste(file_path,file_name,sep = "\\"),height = 11, width = 10)
    grid.table(sgbm1)
    summary(gbm_object,main = "Variable Importance Plot")
    for (i in as.character(summary(gbm_object,plotit = F)$var)) {
      plot(gbm_object,i,type = 'response')
      title(main = paste("Partial Dependence Plot of",toString(i),sep=" "))
    }
    dev.off()
  }
  else {
    warning("file_name parameter coerced to .pdf type")
    pdf(paste(file_path,"\\",file_name,".pdf",sep =""),height = 11, width = 10)
    grid.table(sgbm1)
    summary(gbm_object,main = "Variable Importance Plot")
    for (i in as.character(summary(gbm_object,plotit = F)$var)) {
      plot(gbm_object,i,type = 'response')
      title(main = paste("Partial Dependence Plot of",toString(i),sep=" "))
    }
    dev.off()
  }
}
