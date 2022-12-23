#' The Tuncated Exponential Distribution
#'
#' Bla bla
#'
#' @param n number of observations.
#' @param rate rate of the exponential distribution.
#' @param min value of the lower truncation limit. Default is 0.
#' @param max value of the upper truncation limit. Default is infinite.
#'
#' @details
#' Simple generation of random numbers.
#'
#' @return vector of random deviates.
#'
#' @examples
#'
#' ## A truncated exponential distribution.
#' rate <- 1
#' x <- seq(3,8,by=.1)
#' y <- dtrexp(x,rate=rate,min=3,max=8)
#' z <- ptrexp(x,rate=rate,min=3,max=8)
#' r <- rtrexp(100000,rate=rate,min=3,max=8)
#' hist(r,freq=F)
#' points(x,dtrexp(x,rate=rate,min=3,max=8),type="l")
#'
#' @export

rtrexp <- function(n, rate = 1, min = 0, max = NULL) {
  if (rate<=0) stop("rate must be > 0")
  if (!is.null(max)) if (max <= min) stop("max must be > min")
  p <- runif(n)
  e.min <- exp(-rate*min)
  e.max <- ifelse(is.null(max),0,exp(-rate*max))
  return(-1/rate*log(e.min-p*(e.min-e.max)))
}
