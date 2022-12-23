#' Random vector of integers with fixed sum
#'
#' @param n
#' @param ntot
#'
#' @return
#' A vector of length \code{n} containing random integers whose total sum is
#' equal to \code{ntot}.
#'
#' @export
#'
#' @examples
#' m <- rand_vect_total(8,25)
#'
#' # Total sum is always 25.
#' q <- replicate(100000,rand_vect_total(8,25))
#' summary(apply(q,2,sum))
#'
#' # All numbers are equiprobable.
#' plot(apply(q,1,mean)-25/8,xlab="Vector element",ylab="Deviation about 25/8")

rand_vect_total <- function(n, ntot) {

  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  mf <- match(c("n", "ntot"), names(mf), 0L)
  if (length(mf) != 2) stop("Wrong number of inputs")

  if (n <= 0) stop("Input 'n' must be larger than 0")
  if (ntot <= 0) stop("Input 'ntot' must be larger than 0")
  if (round(n) != n) stop("Input 'n' must be integer")
  if (round(ntot) != ntot) stop("Input 'ntot' must be integer")

  p <- c(0,cumsum(runif(n)))
  p <- round(p/max(p)*ntot)
  return(diff(p))
}
