#' Compute the Poisson log - likelihood between epid and epidemic simulated
#' with R and GT
#' 
#' Compute the Poisson log - likelihood between \code{epid} and epidemic
#' simulated with R and \code{GT}.
#' 
#' For internal use. Called by \code{\link{est.R0.ML}}.
#' 
#' For internal use. Called from est.ML.R0.  Compute the Poisson likelihood of
#' epidemic.
#' 
#' @param log.R log Reproduction ratio
#' @param epid epidemic
#' @param GT Generation time distribution.
#' @param import Vector of imported cases.
#' @param pred Returns either the predictive curve or the log-likelihood value
#' (default)
#' @param offset to \code{offset} (confidence interval)
#' @return Returns a Poisson log-likelihood with given R and \code{GT}
#' @author Pierre-Yves Boelle, Thomas Obadia
#' @keywords internal
#' 
#' @importFrom stats dpois
fit.epid <- function
(log.R,
epid,
GT, 
import, 
pred=FALSE, 
offset=0 
)
{
  ## Compute the Poisson likelihood of epidemic.
	R = exp(log.R)
  T = length(epid$incid)
	GT = GT$GT
	
  #Simulated epidemic is initiated at 0 and has a length of T+length(GT).
  #This way, we can multiply by GT even with the last value of incidence.
  sim.epid = rep(0, T+length(GT))
  
	for (t in 1:T) {
		sim.epid[t:(t+length(GT)-1)] = sim.epid[t:(t+length(GT)-1)] + R * epid$incid[t] * GT
	}

	sim.epid = sim.epid[2:T]
	logV = sum(dpois(epid$incid[2:T]-import[2:T],lambda=sim.epid,log=T))
  
	if (pred==TRUE) {
    return(pred=c(epid$incid[1],sim.epid))
  }
  
  #Most commonly used output is logV
	else {
    return(logV-offset)
	}
  
}

#' Joint estimation of GT distribution and R
#' 
#' Compute the Poisson log - likelihood between epid and epidemic simulated
#' with R and GT.
#' 
#' For internal use. Called by \code{\link{est.R0.ML}}. This is a wrapper
#' function used to pass proper arguments to \code{\link{fit.epid}} when the ML
#' method is used to estimate simultaneously R and GT. It is used by the
#' \code{optim} routine to find the best-fitting parameters for R and GT
#' (following a Gamma dsitribution)
#' 
#' @param par vector of parameters to be optimised. This should be provided as
#' c(R0, GT.mean, GT.sd)
#' @param \dots parameters passed to inner functions
#' @author Pierre-Yves Boelle, Thomas Obadia
#' @keywords internal
fit.epid.optim <- function
( par=c(1,1,1),
  ... 
) 
{
  GT <- generation.time("gamma", c(par[2], par[3]))
  return(fit.epid(log.R=par[1], GT=GT, ...))
}

