#' Title
#'
#' @param x
#' @param w
#' @param type.wrap
#' @param average
#'
#' @return
#' @examples
#' x <- 0:100
#' y <- sin(x/100*2*pi)
#' plot(x, y)
#' points(x, smooth_boxcar(y, 5),type="l")
#' points(x, smooth_boxcar(y, 15),type="l")
#' points(x, smooth_boxcar(y, 25),type="l")
#' points(x, smooth_boxcar(y, 35),type="l")
#'
#' # Smoothing repeatedly woth boxcar converges to a Gaussian.
#' x <- seq(-500, 500, by = 1)
#' y <- numeric(1001)
#' y[501] <- 1
#' sy <- y
#' plot(0, 1, xlim = c(-50,50), pch = 16, cex = 2, ylim = c(0,1))
#' for (i in 1:10) {
#'   sy <- smooth_boxcar(sy, 15)
#'   points(x, sy*15, type = "l")
#' }
#'
#' @export
#'
smooth_boxcar <- function(x, w, type.wrap = NULL, average = T) {

  if (!is.vector(x)) stop("Input 'x' must vector")
  if (is.null(w)) stop("Value for 'w' must be specified")
  if (w<=0) stop(" Value of 'w' must be strictly positive")
  if (round(w) != w) stop("Value of 'w' must be an integer number")
  if (w %% 2 == 0) stop("Value of 'w' must be an odd number")
  nx <- length(x)
  w2 <- (w-1)/2

  # Convolution loop.
  y <- numeric(nx)
  y[w2+1] <- sum(x[1:w])
  ilow <- 1
  iup <- w + 1
  for (i in (w2+2):(nx-w2)) {
    y[i] <- y[i-1] - x[ilow] + x[iup]
    ilow <- ilow + 1
    iup <- iup + 1
  }

  if (average) y <- y/w

  return(y)
}
