#' Bliss test for independence between proportions
#'
#' Bla bla
#'
#' @param ndead a numeric vector with 3 elements, containing number of dead individuals under
#         treatment A, B and combined.
#' @param ntot a numeric vector with 3 elements, containing total number of individuals under
#         the 3 treatments.
#' @param conf.level significance level (default conf.level = 0.05).
#' @param alternative a character string specifying the alternative hypothesis. It must be one of the following:
#' "two.sided", "greater" (default) or "less".
#'
#' @details
#'
#' @return A list with class "htest" containing the following components:
#'
#' @examples
#'
#' @references Sgolastra, F., Medrzycki, P., Bortolotti, L., Renzi, M.T., Tosi, S., Bogo, G., Teper, D., Porrini,
#' C., Molowny‐Horas, R. and Bosch, J., 2017. Synergistic mortality between a neonicotinoid insecticide and an
#' ergosterol‐biosynthesis‐inhibiting fungicide in three bee species. Pest Management Science, 73(6), pp.1236-1243.
#'
#' @export

bliss_indep_propor <- function(ndead, ntot, p.signif=0.05, alternative="greater") {

  # Two-tailed test?
  if (alternative=="two.sided") p.signif <- p.signif/2


  ndead <- unname(ndead)
  ntot <- unname(ntot)
  p <- ndead/ntot
  pa <- p[1]
  pb <- p[2]
  pab.obs <- p[3]
  vara <- p[1]*(1-p[1])/ntot[1]
  varb <- p[2]*(1-p[2])/ntot[2]
  varab.obs <- p[3]*(1-p[3])/ntot[3]
  pab.exp <- pa+pb-pa*pb
  varab.exp <- vara+varb+pb^2*vara+pa^2*varb    # Derived with the Delta method.
  p.dif <- pab.obs-pab.exp
  sd.all <- sqrt(varab.obs+varab.exp)
  z <- qnorm(1-p.signif)
  out <- list(pA=pa,pB=pb,pAB.obs=pab.obs,pAB.exp=pab.exp,p.Dif=p.dif,
              VarA=vara,VarB=varb,VarAB.obs=varab.obs,VarAB.exp=varab.exp,Var.All=sd.all^2,
              CI=switch(alternative,
                        two.sided=c(lower=p.dif-z*sd.all,upper=p.dif+z*sd.all),
                        less=c(upper=p.dif+z*sd.all),
                        greater=c(lower=p.dif-z*sd.all)))
  return(out)
}

