#' Conversion between mean and standard deviations of truncated and normal distributions.
#'
#' It calculates the first and second moments of a truncated normal distribution when the first and second moments of the
#' original normal distribution are known. Conversely, it computes the first and second moment of the original normal
#' distribution from the first and second moments of a truncated normal distributions. In both cases, lower and/or upper
#' thresholds are assumed to be known.
#'
#' @param mean mean of the normal/truncated distribution.
#' @param sd standard deviation of the normal/truncated distribution.
#' @param a lower threshold fo the truncated distribution.
#' @param b upper threshold for the truncated distribution.
#' @param type numerical (see Details below).
#'
#' @details
#' Straightforward implementation of equations relating analytic first and second moments from both distributions.
#' The inverse procedure from truncated parameters to original normal distribution may fail to converge in some cases.
#'
#' If type=1, mean and sd inputs are assumed to be from a normal distribution whose domain is the whole set of real numbers.
#' If type=2, mean and sd inputs are from a truncated normal distribution.
#' If type=3, input mean is from a truncated distribution and input sd is from a normal distribution.
#' If type=4, input mean is from a normal distribution and input sd is from a truncated distribution
#'
#' @return Named vector containing mean and standard deviation.
#'
#' @examples
#'
#' ## We know the normal distribution from which the truncated distribution has been computed from:
#' r <- rnorm(100000,mean=4.2,sd=2.7)
#' rr <- r[r>3.5 & r<8.7]
#' mean_sd_truncnorm(4.2,2.7,a=3.5,b=8.7,type=1)
#' c(mean.truncated=mean(rr),sd.truncated=sd(rr))
#'
#' ## We just have a truncated normal from an unknown normal (although the truncation points must be known!):
#' mean_sd_truncnorm(mean(rr),sd(rr),a=3.5,b=8.7,type=2)
#' c(mean=4.2,sd=2.7)
#'
#' ## We know the mean of the truncated distribution and the std. dev. of the normal distribution.
#' mean_sd_truncnorm(mean(rr),2.7,a=3.5,b=8.7,type=3)
#' c(mean=4.2,sd=sd(rr))
#'
#' ## We know the mean of the normal distribution and the std. dev. of the truncated distribution.
#' mean_sd_truncnorm(4.2,sd(rr),a=3.5,b=8.7,type=4)
#' c(mean=mean(rr),sd=2.7)
#'
#' @export

mean_sd_truncnorm <- function(mean, sd, a = -Inf, b = +Inf, type = 1) {

  if (!any(type == c(1,2,3,4))) stop("Wrong value for 'type' argument")
  if (a >= b) stop("Lower threshold 'a' must be < upper threshold 'b'")
  if (sd < 0) stop("Standard deviation must be a positive number")

  # Parameters to be used in the equations below.
  alpha <- (a-mean)/sd
  beta <- (b-mean)/sd
  da <- dnorm(alpha)
  db <- dnorm(beta)
  ada <- alpha*da
  ada[is.infinite(alpha)] <- 0
  bdb <- beta*db
  bdb[is.infinite(beta)] <- 0
  z <- pnorm(beta)-pnorm(alpha)

  if (type == 1) {
    m <- mean - sd*(db-da)/z
    s <- sd*sqrt(1-(bdb-ada)/z-((db-da)/z)^2)
  } else {

    # Finding the solution with a root-solver. Notation below is such that (m,s) refer to
    # mean and standard deviation of the truncated distribution, whereas (mo,so) refer to
    # mean and standard deviation of the normal distribution.

    f <- function(q,parms) {
      if (type == 2) {
        m <- q[1]
        s <- q[2]
        mo <- parms[3]
        so <- parms[4]
      } else if (type == 3) {
        m <- q[1]
        so <- q[2]
        mo <- parms[3]
        s <- parms[4]
      } else {
        mo <- q[1]
        s <- q[2]
        m <- parms[3]
        so <- parms[4]
      }
      a <- parms[1]
      b <- parms[2]
      alpha <- (a-m)/s
      beta <- (b-m)/s
      da <- dnorm(alpha)
      db <- dnorm(beta)
      ada <- alpha*da
      ada[is.infinite(alpha)] <- 0
      bdb <- beta*db
      bdb[is.infinite(beta)] <- 0
      z <- pnorm(beta)-pnorm(alpha)
      fmean <- m - s*(db-da)/z - mo
      fsd2 <- s^2*(1-(bdb-ada)/z-((db-da)/z)^2) - so^2

      return(c(fmean,fsd2))
    }
    out <- rootSolve::multiroot(f=f,start=c(mean,sd),verbose=F,parms=c(a=a,b=b,mean=mean,sd=sd))
    m <- out$root[1]
    s <- out$root[2]
  }
  return(c(mean=m,sd=s))
}
