#' Quantile function of a truncated exponential distribution
#'
#' Bla bla
#'
#' @param p vector of probabilities.
#' @param rate rate of the exponential distribution.
#' @param min value of the lower truncation limit. Default is 0.
#' @param max value of the upper truncation limit. Default is infinite.
#'
#' @details
#' Quantiles of the truncated exponential distribution corresponding to
#' probabilities p.
#'
#' @return vector of quantiles.
#'
#' @examples
#'
#' ## A truncated exponential distribution.
#' rate <- 1
#' x <- seq(3,8,by=.01)
#' plot(qtrexp(seq(0,1,by=.01),rate=rate,3,8))
#'
#' @export

qtrexp <- function(p, rate = 1, min = 0, max = NULL) {
  if (rate<=0) stop("rate must be > 0")
  if (!is.null(max)) if (max <= min) stop("max must be > min")
  e.min <- exp(-rate*min)
  e.max <- ifelse(is.null(max),0,exp(-rate*max))
  return(-1/rate*log(e.min-p*(e.min-e.max)))
}
