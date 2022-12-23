#' Smoothing of all rows in an array with a running window
#'
#' It implements a
#'
#' @param x matrix to smooth the rows of.
#' @param w vector to smooth the rows of x with.
#' @param verbose logical. If set to TRUE, simple information about progress is shown on console. Default is FALSE.
#' @param wrap logical. If set to TRUE, it wraps around the two ends of each row.
#'
#' @details
#' Some details.
#'
#' @return A smoothed version of input 'x'.
#'
#' @examples
#'
#' ## An example function.
#'
#' @export

smoothx <- function(x, w, verbose = F, wrap = F) {

  if (!is.vector(w)) stop("w must be a vector")
  nw <- length(w)
  if (nw == 1) {
    warning("Width is a scalar. Returning a copy of input")
    return(x)
  }
  if (as.integer(nw/2)*2 == nw) stop("Length of 'w' must be an odd number")

  nr <- NROW(x)
  nc <- NCOL(x)
  if (nr < nw) stop("Length of 'w' must be equal to, or less than, the number of columns of 'x'")
  if (nr == 1) {
    warning("'x' is a one-row matrix. Returning a copy of input")
    return(x)
  }

  hw <- (nw-1)/2

  if (is.vector(x)) {

  }
  y <- matrix(0, nr, NCOL(x), dimnames= list(rownames(x), colnames(x)))
  for (i in 1:nr) {
    if (verbose) if (round(i/100) == i/100) cat(paste0("Step ",i," of ",nr,"...\n"))
    x.low <- max(c(1,i-hw))
    x.up <- min(c(nr,i+hw))
    w.low <- ifelse((i-hw) < 1, 1-(i-hw)+1, 1)
    w.up <- ifelse((i+hw) > nr, nr-(i-hw)+1, nw)
    y[i,] <- w[w.low:w.up] %*% x[x.low:x.up,]
  }

  return(y)
}
