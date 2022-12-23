#' Fast kernel smoothing.
#'
#' It returns the binned kernel density estimate for a discrete set of points.
#'
#' @param x vector of abscissae values where density will be evaluated.
#' @param y vector with a set of points. Length of x and y need not be the same.
#' @param type string that determines which smoothing kernel to use.
#' At the moment only type = "gaussian" has been implemented.
#' @param width If type = "gaussian", width is the standard deviation of the gaussian kernel.
#'
#' @details
#'
#' @return value of mode
#'
#' @examples
#'
#' ## Simple example
#'
#' @export

fast_kernsmooth <- function(x, y, type = "gaussian", width = NULL) {

  if (is.null(width)) stop("Please supply width")
  if (width <= 0) stop("width must be >0")

  xmin <- min(x)
  xmax <- max(x)
  if (type == "gaussian") {
    denom <- pnorm((xmax-y)/width) - pnorm((xmin-y)/width)
    w <- width^2
    w2 <- 2*w
    z <- colSums(exp(-outer(y,x,"-")^2/w2)/denom)/(sqrt(2*pi)*width)
  } else if (type == "lognormal") {
    denom <- pnorm((xmax-y)/width) - pnorm((xmin-y)/width)
    w <- width^2
    w2 <- 2*w
    z <- colSums(exp(-outer(y,x,"-")^2/w2)/denom)/(sqrt(2*pi)*width)
  } else {
    z <- y
  }
  return(z)

}
