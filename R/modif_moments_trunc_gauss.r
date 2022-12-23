#' Weights and abscissae...
#'
#' It implements
#'
#' @param x
#' @param a
#'
#' @details
#'
#' @return
#'
#' @examples
#'
#' ## An example function.
#'
#' @export

modif_moments_trunc_gauss <- function(x, mean = mean, sd = sd, limits = limits) {

  P <- function(x, n) {
    if (n<0) stop("Degree of Legendre polynomial cannot be negative")
    if (round(n) != n) stop("Degree of Legendre polynomial must be a natural number")
    if (n==0) {
      return(1)
    } else if (n==1) {
      return(x)
    }

  }



}
