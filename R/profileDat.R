prof_dat <- function (x) 
{
  pro_df <- data.frame(row_num = 1:ncol(x), col_name = names(x))
  pro_df$row_cnt <- nrow(x)
  pro_df$uniq_val <- sapply(x, function(x) length(unique(x)))
  pro_df$na_cnt <- sapply(x, function(x) sum(is.na(x)))
  pro_df$blank_cnt <- sapply(x, function(x) length(x[!is.na(x) & 
                                                       as.character(x) == ""]))
  pro_df$top_val <- sapply(x, function(x) {if (!all(is.na(x))) return(rankTab(x)[1, "x"]) else return(NA)})
  pro_df$na_pct <- round(pro_df$na_cnt/pro_df$row_cnt, 4)
  pro_df$blank_pct <- round(pro_df$blank_cnt/pro_df$row_cnt, 
                            4)
  pro_df$top_5_val_pct <- round(sapply(x, function(x) {if (all(is.na(x))) v = length(x) 
  else {
    if (nrow(rankTab(x)) < 5) v = sum(sum(rankTab(x)[1:nrow(rankTab(x)), "Freq"], na.rm = TRUE))
    else v = sum(sum(rankTab(x)[1:5, "Freq"], na.rm = TRUE)) }
  return(v/length(x))}), 4)
  return(pro_df)
}