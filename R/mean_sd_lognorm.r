#' First and second moment of normal distribution from set of log-normally distributed data points, or viceversa.
#'
#' It calculates the mean and standard deviation of a lognormal distribution given the mean and standard deviation of
#' a set of normally distributed points. Conversely, it computes the mean and standard deviation of a normal distribution
#' given the mean and standard deviation of a set of normally-distributed points. It also calculates mean and standard deviation
#' for other combinations of input data.
#'
#' @param mean mean of the normal/truncated distribution.
#' @param sd standard deviation of the normal/truncated distribution.
#' @param type numerical (see Details below)..
#'
#' @details
#' Straightforward implementation of equations relating analytical first and second moments from both distributions.
#'
#' @return Named vector containing the mean and standard deviation.
#'
#' @examples
#'
#' ## A simple example: from normal to lognormal.
#' x <- rlnorm(100000,meanlog=-1.2,sdlog=1.4)
#' mean_sd_lognorm(-1.2,1.4,type=1)
#' c(mean=mean(x),sd=sd(x))
#'
#' ## From lognormal to normal.
#' mean_sd_lognorm(mean(x),sd(x),type=2)
#' c(mean=-1.2,sd=1.4)
#'
#' @export

mean_sd_lognorm <- function(mean, sd, type = 1) {

  # mc <- match.call()
  # m <- match(c("x", "y", "input_is_normal"),names(mc), NA)
  #
  # if (!any(is.na(m[1:2]))) {
  #   if (length(x) > 1 | length(y) > 1) stop("x and y must be numbers")
  #   mo <- x
  #   vo <- y^2
  # } else {
  #   if (length(x) == 1) stop("Length of vector x must be larger than 1")
  #   mo <- mean(x)
  #   vo <- sd(x)^2
  # }

  v <- sd^2
  if (type == 1) {              # Assuming x (and y) correspond to a normal distribution
    m <- exp(mean+v/2)
    v <- exp(2*mean+v)*(exp(v)-1)
  } else if (type == 2) {       # Assuming x (and y) correspond to a log-normal distribution
    v <- log(1+v/mean^2)
    m <- log(mean)-v/2
  }

  return(c(mean=m,sd=sqrt(v)))

}
