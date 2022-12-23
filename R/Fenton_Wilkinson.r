#' Fenton-Wilkinson approximation
#'
#' It applies the Fenton-Wilkinson approximation for the convolution of L i.i.d. log-normal random variables.
#'
#' @param mean sample mean
#' @param sd sample standard deviation
#' @param L number of i.i.d. log-normal distributions
#' @param direction logical. If set to TRUE, it calculates the mean and standard deviation of each single i.i.d.
#' log-normal component. If FALSE, it computes the mean and standard deviation of the sum of L i.i.d. log-normal
#' distributions.
#'
#' @details The aproximation assumes that the sum of those variables is itself another log-normally
# distributed random variable, for which this function calculates the mean and standard
# deviation. Alternatively, we can compute the mean and standard deviation of each i.i.d.
# log-normal random variable if we are given the mean and standard deviation of the sum.
# These options are governed by the "inverse" logical variable.
#'
#'
#' @return A named vector containing the mean and standard deviation of the approximation
#'
#' @examples
#'
#'
#' @references
#'
#' @export


# Fenton-Wilkinson approximation for the convolution of L i.i.d. log-normal random variables.

# Options:
#   inverse=F (default):  computes the mean and standard deviation of the sum.
#   inverse=T:            computes the mean and standard deviation of each single i.d.d. component.
Fenton_Wilkinson <- function(mean = Mean, sd = Sd, L = L, inverse=F) {
  if (!inverse) {
    vx <- log((exp(Sd^2)-1)/L+1)
    sx <- sqrt(vx)
    mx <- log(L*exp(Mean))+(Sd^2-vx)/2
  } else {
    vx <- log(L*(exp(Sd^2)-1)+1)
    sx <- sqrt(vx)
    mx <- Mean-(vx-Sd^2)/2-log(L)
  }
  return(data.frame(meanlog=mx,sdlog=sx))
}
