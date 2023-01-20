#' Density of truncated exponential distribution
#'
#' Density for the truncated exponential distribution, given its rate and the lower and/or lower truncation limits.
#'
#' @param x vector of quantiles.
#' @param rate rate of the exponential distribution.
#' @param min value of the lower truncation limit. Default is 0.
#' @param max value of the upper truncation limit. Default is infinite.
#'
#' @details
#' Simple calculation of the density of the truncated exponential distribution.
#'
#' @return density of the truncated exponential distribution.
#'
#' @examples
#'
#' ## A truncated exponential distribution.
#' rate <- .1
#' x <- seq(3,8,by=.01)
#' y <- dtexp(x,rate=rate,min=3,max=8)
#' plot(x,y,type="l")
#'
#' # Its integral must be =1 for y to be a distribution.
#' library(MiscMath)
#' quad_ext_simpson(y,x[2]-x[1])
#'
#' @export

bootstrap_prediction_interval <- function(data=dat, mod_family = "gaussian", mod_link = NULL, nboot = 100, prob = NULL)


  a=-2
  b=.3
  x.by <- .1
  x=seq(x.by,10,by=x.by)
  r <- rnorm(length(x))
  y <- a+b*x+r

  p <- 1/(1+exp(y))
  dat <- data.frame(x=x,y=rbinom(length(x),1,p))
  # dat <- data.frame(x=x,y=y)

  nr <- nrow(dat)

  mod_family <- "binomial"
  # mod_family <- "gaussian"
  mod_link <- NULL

  nboot <- 1000

  if (is.null(prob)) {
    prob <- c(.025,.975)
  } else {
    if (length(prob) != 2) {
      stop("'prob' must be a two-element vector")
    } else {
      if (any(prob > 1 | prob < 0)) stop("'prob' values must be >=0 and <=1")
    }
  }

  if (is.null(mod_link)) {
    mod_link <- switch(mod_family,
                       binomial = "logit",
                       gaussian = "identity",
                       Gamma <- "inverse",
                       inverse.gaussian = "1/mu^2",
                       poisson = "log",
                       quasi = "identity",
                       quasibinomial = "logit",
                       quasipoisson = "log"
    )
  }

  sim <- replicate(nboot, {
    j <- sample(nr,replace=T)
    reg <- switch(mod_family,
                  binomial = glm(y~., data=dat[j,], family = binomial(link = mod_link)),
                  gaussian = glm(y~., data=dat[j,], family = gaussian(link = mod_link)),
                  Gamma = glm(y~., data=dat[j,], family = Gamma(link = mod_link)),
                  inverse.gaussian = glm(y~., data=dat[j,], family = inverse.gaussian(link = mod_link)),
                  poisson = glm(y~., data=dat[j,], family = poisson(link = mod_link)),
                  quasi = glm(y~., data=dat[j,], family = quasi(link = mod_link)),
                  quasibinomial = glm(y~., data=dat[j,], family = quasibinomial(link = mod_link)),
                  quasipoisson = glm(y~., data=dat[j,], family = quasipoisson(link = mod_link))
    )
    predict(reg, type = "response") - dat$y[j]
  })

  s <- apply(sim,1,sd)

  q <- sapply(dat, function(a) {

  })


  s<-apply(fitboot,1,sd)
  reg <- glm(y~x,data=dat,family=mod_family)
  py <- predict(reg,type="response")
  plot(x,dat$y,pch=16,ylim=c(0,1))
  plot(x,dat$y)
  points(x,py,type="l",lwd=2)
  py.down <- predict(reg,type="link")+qnorm(.025)*s
  py.up <- predict(reg,type="link")+qnorm(.975)*s

  points(x,1/(1+exp(-py.down)),type="l",lwd=2,lty=2)
  points(x,1/(1+exp(-py.up)),type="l",lwd=2,lty=2)

  # points(x,py.down,type="l",lwd=2,lty=2)
  # points(x,py.up,type="l",lwd=2,lty=2)

  print((sum(dat$y<(py+qnorm(.025)*s)) + sum(dat$y>(py+qnorm(.975)*s)))/length(x))


  return(y)
}
