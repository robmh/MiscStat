#' Z-test between two means
#'
#' @description
#' It applies the two-sample Z-test when only the means and the corresponding standard errors of the two samples are known.
#'
#' @param mx a number indicating the expected values for first dataset.
#' @param my a number indicating the expected values for second dataset.
#' @param sx a number indicating the standard error for first dataset.
#' @param sy a number indicating the standard error for second dataset.
#' @param alternative a character string specifying the alternative hypothesis. It must be one of the following:
#' "two.sided" (default), "greater" or "less".
#'
#' @details The Z-test can be applied when only the expected values and corresponding standard errors of two samples are known.
#' A typical example would be the comparison of similar coefficients (e.g. the slopes) from two different linear regression models
#' to figure out whether they are significantly different.
#'
#' The specification of the alternative hypothesis takes mx as reference. That is, if we specify alternative="less", our
#' alternative hypothesis is that mx is less than my.
#'
#' @return A list with class "htest".
#'
#' @examples
#'
#' ## Simple example
#' ztest_twomeans(.3,.5,.2,.15)
#'
#' @references Clogg, Clifford C., Eva Petkova, and Adamantios Haritou.
#' "Statistical methods for comparing regression coefficients between models." American Journal of Sociology 100.5 (1995): 1261-1293.
#'
#' @export

ztest_twomeans <- function(mx, my, sx, sy, alternative = "two.sided") {

  if (!any(alternative == c("two.sided","greater","less"))) stop("Wrong alternative choice")

  z <- (mx-my) / sqrt(sx^2+sy^2)
  if (alternative == "two.sided") {
    p.value <- (1-pnorm(abs(z)))*2
  } else if (alternative == "greater") {
    p.value <- pnorm(-z)
  } else p.value <- pnorm(z)

  # Build output
  names(z) <- "Z"
  name.x <- deparse(substitute(mx))
  name.sx <- deparse(substitute(sx))
  name.y <- deparse(substitute(my))
  name.sy <- deparse(substitute(sy))
  out <- list(method="Z-test between two means",
              statistic=z,
              data.name=paste(name.x,",",name.sx," and ",name.y,",",name.sy,sep=""),
              p.value=unname(p.value),
              alternative=switch(alternative,
                                 "greater"= paste(name.x," is greater than ",name.y,sep=""),
                                 "less"=paste(name.x," is less than ",name.y,sep=""),
                                 paste(name.x," is different from ",name.y,sep="")))
  attr(out,"class") <- "htest"
  return(out)
}
