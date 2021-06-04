#' Plot Factor Variables from GBM Object based on prediction & sized by frequency in data set
#'
#' @param gbm_obj - GBM Object
#' @param col_name - character string matching one of the Variables used in the GBM Object
#' @param df - data frame to count frequency of Variable occurrences

#' @return a ggplot2 plot with x-axis of each factor level sorted based on expected prediction & point size based on frequency in data
#' @export

#' @examples

#' df1 <- iris
#' df1$hi_sl <- 0
#' df1$hi_sl[df1$Sepal.Length > 5.8] <- 1 # median sepal length = 5.8

#' vtu1 <- c('Sepal.Width'
#'           ,'Petal.Length'
#'           ,'Petal.Width'
#'           ,'Species'
#'           )
#' # Fit a GBM
#' set.seed(102) # for reproducibility
#' gbm1 <- gbm(df1$hi_sl ~.,  df1[,vtu1]
#'             , distribution = "bernoulli"
#'             , n.trees = 100
#'             , shrinkage = 0.1
#'             , interaction.depth = 3
#'             , cv.folds = 3
#'             , keep.data = FALSE
#'             , verbose = FALSE
#'             )
#'
#' gbm_plot_factor(gbm1, 'Species',df1)


gbm_plot_factor <- function(gbm_obj,col_name, df){
  require(ggplot2)
  require(forcats)
  df_x <- as.data.frame(plot(gbm_obj,col_name)$panel.args)
  df_x$x <- as.character(df_x$x)
  tab1 <- as.data.frame(table(df[,col_name]))
  df_x$count1 <- tab1$Freq[match(tab1$Var1, df_x$x)]
  #df_x <- df_x[order(df_x$y),]
  #df_x$x <- as.character(df_x$x )
  #df_x$x <- factor(df_x$x, levels=unique(df_x$x))
  df_x$x <- factor(df_x$x, levels = df_x$x[order(df_x$y)])

  print(head(df_x))
  p <- ggplot(df_x, aes(x=x, y=y)) + geom_point(aes(size = count1))
  p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=.5, size = 8))
  p
}




