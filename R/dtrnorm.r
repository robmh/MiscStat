#' Density of truncated normal distribution
#'
#' Density for the truncated normal distribution, given its mean, sd and the lower and/or lower truncation limits.
#'
#' @param x vector of quantiles.
#' @param mean mean of the standard normal.
#' @param sd standard deviation of the standard normal.
#' @param a optional; lower truncation limit. Default is -Inf.
#' @param b optional; upper truncation limit. Default is +Inf.
#' @param log logical; if TRUE, probabilities are given in logarithmic scale.
#'
#' @details
#' Simple calculation of the density of the truncated standard normal distribution. For those values of \code{x} that
#' are outside the interval \code{[a,b]} are set to zero whether \code{log} is TRUE or FALSE.
#'
#' @return density of the truncated standard normal distribution.
#'
#' @examples
#'
#' dtrnorm(seq(0,20,by=.01),3,1.7,a=1,b=5,log=F)
#' dtrnorm(seq(0,20,by=.01),3,1.7,a=1,b=5,log=T)
#'
#' @export

dtrnorm <- function(x, mean = 0, sd = 1, a = -Inf, b = Inf, log = FALSE) {
  if (sum(a>=b)>0) stop("Lower and/or upper limits must be such that a<b")
  numer <- dnorm(x, mean = mean, sd = sd, log = log)
  denom <- pnorm(b, mean = mean, sd = sd) - pnorm(a, mean = mean, sd = sd)
  y <- if (log) numer-log(denom) else numer/denom
  y[x<a | x>b] <- 0
  return(y)
}
