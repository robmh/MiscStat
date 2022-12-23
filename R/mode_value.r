#' Mode of vector
#'
#' It calculates the mode of values in vector x.
#'
#' @param x a vector of values.
#'
#' @details It is a simple algorithm that does not work well
#' for multimodal distributions, for example.
#'
#' @return value of mode
#'
#' @examples
#'
#' ## Simple example
#' x <- sample(1:10,100,replace=T)
#' mode_value(x)
#'
#' @export

mode_value <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
