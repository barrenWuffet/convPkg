#' Function to write the variable importance plots of a gbm object to a pdf.
#'
#' @param gbm_object An object containing a GBM model and components
#' @param file_path A character string pointing to the directory of where to write the PDF containing the partial dependency plots.
#' @param file_name A character string pointing to the file of where to write the PDF containing the partial dependency plots.
#'
#' @return NA - writes to file.
#' @export
#' @author Arshan Tarapore
#' \email{aktarapore@@gmail.com}
#'
#' @examples
#'
#'
#'
exportGBMtoPDF <- function(gbm_object,file_path = '.',file_name='var_imp.pdf'){
  if(class(gbm_object) != "gbm")
    stop("gbm_object parameter is not of class'gbm'")
  if(class(file_path) != "character")
    stop("file_path parameter is not of class 'character'")
  if(class(file_name) != "character")
    stop("file_name parameter is not of class 'character'")
  sgbm1 <-summary(gbm_object)
  if(tools::file_ext(file_name)== "pdf") {
    vtu2 <- as.character(summary(gbm_object, plotit = F)$var)
    pdf(paste(file_path,file_name,sep = "/"),height = 11, width = 10)
    summary(gbm_object,main = "Variable Importance Plot")
    for (i in vtu2) {
      plot(gbm_object,i)
      title(main = paste("Partial Dependence Plot of",toString(i),sep=" "))
    }
    dev.off()
  }
  else {
    warning("file_name parameter coerced to .pdf type")
    vtu2 <- as.character(summary(gbm_object, plotit = F)$var)
    pdf(paste(file_path,file_name,sep = "/"),height = 11, width = 10)
    summary(gbm_object,main = "Variable Importance Plot")
    for (i in vtu2) {
      plot(gbm_object,i)
      title(main = paste("Partial Dependence Plot of",toString(i),sep=" "))
    }
    dev.off()
  }
}

