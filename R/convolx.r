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
  hw <- (nw-1)/2

  nr <- NROW(x)
  if (is.vector(x)) {
    if (nr < nw) stop("Length of 'w' must be equal to, or less than, length of vector 'x'")
    nc <- 1
    y <- double(nr)
    if (!is.null(names(x))) names(y) <- names(x)
  } else if (is.matrix(x)) {
    nc <- NCOL(x)
    if (nc < nw) stop("Length of 'w' must be equal to, or less than, number of columns of matrix 'x'")
    y <- matrix(0, nr, nc)
    if (!is.null(rownames(x))) rownames(y) <- rownames(x)
    if (!is.null(colnames(x))) colnames(y) <- colnames(x)
  } else {
    stop("Input 'x' must be a 1D vector or a 2D matrix")
  }



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
