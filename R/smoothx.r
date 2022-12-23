#' Smoothing of every row in an array along columns with a running window
#'
#' Simple modification of base-R function "filter" to make it smooth a vector or array.
#'
#' @param x matrix to smooth the rows of.
#' @param w vector to smooth the rows of x with. Its length must be an odd number.
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

smoothx <- function(x, filter = 3, endrule = "circular") {

  h <- length(filter)
  h2 <- as.integer(h/2)
  if (h2*2==h) stop("Length of 'filter' must be an odd number")

  if (!is.vector(x) & !is.matrix(x)) stop("Input 'x' must be a 1D vector or a 2D matrix")
  if (endrule == "circular") {
    y <- t(filter(t(x),rev(filter),circular=T))
  } else if (endrule == "pad") {
    if (is.vector(x)) {
      nr <- length(x)
      y <- c(rep(x[1],h2),x,rep(x[nr],h2))
      y <- filter(y,rev(filter),circular=F)
      y <- y[(h2+1):(h2+nr)]
    } else {
      nc <- ncol(x)
      y <- x
      for (i in 1:h2) y <- cbind(x[,1],y)
      for (i in 1:h2) y <- cbind(y,x[,nc])
      y <- t(filter(t(y),rev(filter),circular=F))
      y <- y[,(h2+1):(h2+nc)]
    }
  }
  if (is.vector(x)) {
    y <- as.vector(y)
    if (!is.null(names(x))) names(y) <- names(x)
  }
  if (is.matrix(x)) {
    y <- as.matrix(y)
    if (!is.null(rownames(x))) rownames(y) <- rownames(x)
    if (!is.null(colnames(x))) colnames(y) <- colnames(x)
  }
  return(y)
}
