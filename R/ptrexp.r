#' Cumulative distribution function of the truncated exponential distribution
#'
#' Cumulative distribution function for the truncated exponential distribution,
#' given its rate and the lower and/or lower truncation limits.
#'
#' @param x vector of quantiles.
#' @param rate rate of the exponential distribution.
#' @param min value of the lower truncation limit. Default is 0.
#' @param max value of the upper truncation limit. Default is infinite.
#'
#' @details
#' Simple calculation of the cumulative distribution function of the truncated exponential distribution.
#'
#' @return density of the truncated exponential distribution.
#'
#' @examples
#'
#' ## A truncated exponential distribution.
#' rate <- .1
#' x <- seq(3,8,by=.1)
#' y <- dtrexp(x,rate=rate,min=3,max=8)
#' z <- ptrexp(x,rate=rate,min=3,max=8)
#' plot(x,z,type="l")
#'
#' # We check the CDF values by numerical quadrature.
#' library(MiscMath)
#' q <- sapply(2:length(y),function(i) quad_trapez(y[1:i],x[2]-x[1]))
#' points(x[-1],q,type="l",lwd=2,lty=2)
#'
#' @export

ptrexp <- function(x, rate = 1, min = 0, max = NULL) {
  if (rate<=0) stop("rate must be > 0")
  if (!is.null(max)) if (max <= min) stop("max must be > min")
  y <- exp(-rate*min)-exp(-rate*x)
  z <- exp(-rate*min)
  if (!is.null(max)) {
    if (min(x)<min | max(x)>max) stop("x should be min <= x <= max")
    z <- z-exp(-rate*max)
  }
  return(y/z)
}
