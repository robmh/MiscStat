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

window_2d_poly <- function() {
  x <- y <- -1:1
  xy <- expand.grid(x,y)
  f <- function(x,y) c(1,x,x^2,y,y^2,x*y)
  A <- sapply(1:nrow(xy),function(i) f(xy[i,1],xy[i,2]))
  B <- solve(A %*% t(A)) %*% A
  B <- B*36 #(to avoid decimals; the results must be divided by 36).

  yy <- runif(nrow(xy))
  r <- lm(yy~x+I(x^2)+y+I(y^2)+I(x*y),data=data.frame(x=xy[,1],y=xy[,2]))
  p <- predict(r)

  cc <- (B %*% yy)/36




}
