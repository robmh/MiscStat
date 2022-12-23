#' Title
#'
#' @param x
#' @param w
#' @param average
#'
#' @return
#' @export
#'
#' @examples
smooth_boxcar_2D <- function(x, w, average = T) {

  if (!is.matrix(x)) stop("Input 'x' must be a matrix")

  y <- as.vector(x)
  y <- smooth_boxcar(y, w, average = average)
  y <- array(y, dim = dim(x))
  y <- t(y)
  y <- smooth_boxcar(y, w, average = average)
  y <- array(y, dim = dim(x))
  y <- t(y)

  return(y)
}
