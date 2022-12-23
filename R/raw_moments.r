#' Title
#'
#' @param x
#' @param p
#' @param order
#' @param type
#'
#' @return
#' @export
#'
#' @examples
raw_moments <- function(x, p, order = 1, type = "continuous") {

  if (order != round(order)) stop("Input 'order' must be an integer")
  if (order < 0) stop("Input 'order' must be >=0")

  if (type == "continuous") {
    r <- MiscMath::quad_ext_simpson(p*x^order, x[2]-x[1])
  } else {
    r <- sum(p*x^order)
  }

  return(r)
}
