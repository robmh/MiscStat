#' Rate parameter of truncated exponential distribution
#'
#' Given an observed (upper and/or lower) truncated exponential distribution
#' it searches for the rate parameter of the non-truncated distribution.
#'
#' @param min value of the lower truncation limit of the random exponential variable.
#' Default is 0.
#' @param max value of the upper truncation limit of the random exponential variable.
#' Default is infinite. Case min=0 and max=NULL corresponds to a non-truncated exponential distribution.
#' @param mean_value mean value of the observed distribution.
#' @param first_guess initial guess for the lambda parameter. If not supplied, first guess will be 1/mean_value.
#' @param method string to select minimization with nlm (the default) or Newton-Raphson root-finding method.
#' Both results should give fairly similar results.
#'
#' @details
#' R built-in nlm function is first used to find the value of rate that minimizes the log-likelihood.
#' Optionally, that value can be further refined with a Newton-Raphson scheme, although this is usually overkill.
#'
#' @return rate of non-truncated exponential distribution.
#'
#' @examples
#'
#' ## A set of 100 exponentially-distributed points.
#' rate <- .6
#' x <- rexp(100,rate)
#' y <- x[x>1.2 & x<3.9]
#'
#' ## Initial guess is left to the function.
#' rate1 <- rate_truncexp(min=1.2,max=3.9,mean(y))
#'
#' ## Further refinement with Newton-Raphson. Difference with lambda1 should be small.
#' rate2 <- rate_truncexp(min=1.2,max=3.9,mean(y),method="nr")
#'
#' ## Same as above, but with a poor first guess.
#' rate3 <- rate_truncexp(min=1.2,max=3.9,mean(y),first_guess=1)
#' rate4 <- rate_truncexp(min=1.2,max=3.9,mean(y),first_guess=1,method="nr")
#'
#' ## Results are all very similar but Newton-Raphson with a poor first guess.
#' rates <- c('True rate'=rate,Rate1=rate1,Rate2=rate2,Rate3=rate3,Rate4=rate4)
#' print(rates)
#'
#' er <- abs(1-c(Rate1=rate1,Rate2=rate2,Rate3=rate3,Rate4=rate4)/rate)
#' print(er)
#'
#' ## A very poor first guess gives error messages about failure to converge.
#' rate_truncexp(min=1.2,max=3.9,mean(y),first_guess=20)
#'
#' @export

rate_truncexp <- function(min = 0, max = NULL, mean_value, first_guess = NULL, method = "nlm", maxiter = 1000, tol = .Machine$double.eps^0.25) {

  if (!is.null(max)) if (max <= min) stop("max must be > min")
  if (!any(method==c("nlm","nr"))) stop("method must be 'nlm' or 'nr'")
  rate <- ifelse(is.null(first_guess),1/mean_value,first_guess)

  if (is.null(max)) {
    rate <- 1/(mean_value-min)      # Straightforward case.
  } else {

    froot <- function(r) 1/r - mean_value + (min*exp(-r*min)-max*exp(-r*max))/(exp(-r*min)-exp(-r*max))

    if (method == "nlm") {

      # One-dimensional minimization. Unlike optimize, with nlm we only need to specify a starting value.
      froot2 <- function(r) froot(r)^2
      rate <- nlm(froot2, rate, iterlim = maxiter)
      if (rate$code == 4) stop("Iteration limit exceeded")
      if (rate$code == 5) stop("Failed to converge. Converge code = 5. See Details in nlm")
      rate <- rate$estimate

    } else {

      # If Newton-Raphson is chosen, the analytical derivative is required.
      froot_deriv <- function(r) {
        emin <- exp(-r*min)
        emax <- exp(-r*max)
        denom <- emin-emax
        return(-1/rate^2 + (max^2*emax-min^2*emin)/denom+(min*emin-max*emax)^2/denom^2)
      }
      iter <- 1
      repeat {
        rate_old <- rate
        rate <- rate - froot(rate)/froot_deriv(rate)
        if (abs(rate-rate_old) < tol) break
        iter <- iter + 1
        if (iter > maxiter) stop("Newton-Raphson scheme could not converge")
      }
    }
  }

  return(rate)
}
