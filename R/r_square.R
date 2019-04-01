#' Calculate Rsquare for regression/continuous/non-categorical predictions
#'
#' @param pred Model predictions, a vector of predictions 
#' @param obs Observations, a vector of true values/target
#' @param formula, "traditional" or "corr", if traditional(default), then  use r2 formula, if corr, then use correlation formula
#' @param na.rm, if TRUE (default), then remove NAs in calculations
#'
#' @return r2 values based on predictions and observations
#' @export
#'
#' @examples
#' pred <- rnorm(10,0,1)
#' obs <- rnorm(10,0,1)
#' r2(pred, obs)
#' 
#' 
#' @author \itemize{
#'    \item Zach Armentrout - \email{zarmentrout@@alixpartners.com}
#'    \item Qianbo Wang - \email{qwang@@alixpartners.com}
#' }
#'
#'
#'

r2 <- function(pred, obs, formula = "traditional", na.rm = T) {
  n <- sum(complete.cases(pred))
  switch(formula,
         corr = cor(obs, pred, use = ifelse(na.rm, "complete.obs", "everything"))^2,
         traditional = 1 - (sum((obs-pred)^2, na.rm = na.rm)/((n-1)*var(obs, na.rm = na.rm))))
}
