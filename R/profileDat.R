
#' Standard method of profiling a data frame
#'
#' @param x a data.frame
#'
#' @return data.frame with profiling data from input data.frame
#' @export
#'
#' @examples
#' prof_dat(iris)
#'
prof_dat <- function(x){
  pro_df <- data.frame(row_num = 1:ncol(x), col_name = names(x))
  pro_df$row_cnt <- nrow(x)
  # Find unique counts
  pro_df$uniq_val <- sapply(x, function(x) length(unique(x)))
  # Find NA's
  pro_df$na_cnt <- sapply(x, function(x) sum(is.na(x)))
  # Find blanks
  pro_df$blank_cnt <- sapply(x, function(x) length(x[!is.na(x) & as.character(x) == '']))
  pro_df$top_val <- sapply(x, function(x) {if (!all(is.na(x))) return(rankTab(x)[1, "x"]) else return(NA)})
  
  pro_df$na_pct <- round(pro_df$na_cnt / pro_df$row_cnt, 4)
  pro_df$blank_pct <- round(pro_df$blank_cnt / pro_df$row_cnt, 4)
  # pro_df$top_5_val_pct <- round(sapply(x, function(x) sum(!(is.na( rankTab(x)[1:max(5,nrow(x)), "Freq"]))) / nrow(x)), 4)
  pro_df$top_5_val_pct <- round(sapply(x, function(x) {if (all(is.na(x))) length(x) else {
    if (nrow(rankTab(x)) < 5) sum(sum(rankTab(x)[1:nrow(rankTab(x)), "Freq"], na.rm = TRUE))
    else sum(sum(rankTab(x)[1:5, "Freq"], na.rm = TRUE)) }}/length(x)), 4)
  pro_df$top_1_val_pct <- round(sapply(x, function(x) {if (all(is.na(x))) length(x) else sum(sum(rankTab(x)[1, "Freq"], na.rm = TRUE))} / length(x)), 4)
  
  # sum(!(is.na( rankTab(x)[1:max(5,nrow(x)), "Freq"])))
  return(pro_df)
}