#' A Memory Object Browser Function
#' Shamelessly Copied from this stackoverflow.com post
#' http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
#'

#' @param ... Passed to .ls.objects
#' @param n Number of objects to show
#' @keywords memory
#' @export
#' @examples
#' data(iris)
#' lsos()
###
# improved list of objects

# shorthand
lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
