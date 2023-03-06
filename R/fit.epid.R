# Name   : fit.epid
# Desc   : Computes a fitted model that accomodates the incidence values provided
#          in the Maximum Likelihood method implementation
# Date   : 2011/11/09
# Update : 2023/03/02
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Poisson log-likelihood for an observed epidemic
#' 
#' @description
#' Computes the Poisson log-likelihood of an observed epidemic, compared to the
#' expected incidence values for a given R and generation time distirbution.
#' 
#' @note
#' This function is also used in [fit.epid.optim()], which is a wrapper for the
#' joint optimization routine to determine the best-fitting R and GT values when
#' the [est.R0.ML()] method is called and asked to jointly estimate reproduction 
#' ratios and generation time distirbution.
#' 
#' @details
#' For internal use. Called from [est.R0.ML()].	
#' 
#' Computes and returns the Poisson log-likelihood of an observed epidemic, 
#' conditional on a given value of R and a generation time distribution. 
#' 
#' @param log.R The log-reproduction ratio.
#' @param epid An epidemic curve, in the sense of passing [check.incid()] validations. It must have at least an incidence vector in an `incid` component.
#' @param GT Generation time distribution from [generation.time()]. 
#' @param import Vector of imported cases
#' @param pred Boolean. By default (`FALSE`), the function returns the corresponding log-likelihood. When set to `TRUE`, the function will return the Poisson-predicted incidence series instead.
#' @param offset Offset value for the log-likelihood (used to calculate confidence intervals).
#' 
#' @return
#' The Poisson log-likelihood of the observed epidemic passed as argument `epid`, 
#' or the Poisson-predicted incidence series given R and GT values.
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

fit.epid <- function(
    log.R, 
    epid, 
    GT, 
    import, 
    pred    = FALSE, 
    offset  = 0 
)
  
  # Code
  
{
  R <- exp(log.R)
  T <- length(epid$incid)
  GT <- GT$GT
  
  #Simulated epidemic is initiated at 0 and has a length of T+length(GT).
  #This way, we can multiply by GT even with the last value of incidence.
  sim.epid <- rep(0, T+length(GT))
  
  for (t in 1:T) {
    sim.epid[t:(t+length(GT)-1)] <- sim.epid[t:(t+length(GT)-1)] + R * epid$incid[t] * GT
  }
  
  sim.epid <- sim.epid[2:T]
  logV <- sum(dpois(epid$incid[2:T]-import[2:T],lambda=sim.epid,log=T))
  
  if (pred == TRUE) {
    return(pred = c(epid$incid[1],sim.epid))
  }
  
  #Most commonly used output is logV
  else {
    return(logV-offset)
  }
  
}





#' @title
#' Joint estimation of R and generation time distribution for the ML method
#' 
#' @description
#' An objective function, wrapped around [fit.epid()], that will be passed to
#' [stats::optim()] to jointly estimate the best-fitting R and GT distributions
#' in the context of the Maximum-Likelihood method from [est.R0.ML()].
#' 
#' @details
#' For internal use. Called from [est.R0.ML()].	
#' 
#' This is a wrapper function used to pass proper arguments to [fit.epid()] when 
#' the ML method is used to estimate simultaneously R and GT. This function is 
#' used as objective to maximize by [stats::optim()].
#' 
#' @param par A vector of three numerical values to be optimized, in the order `c(log.R, GT$mean, GT$sd)`. 
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' A Poisson log-likelihood from [fit.epid()].
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

fit.epid.optim <- function(
    par = c(1,1,1),
    ... 
) 
  
  # Code
  
{
  GT <- generation.time("gamma", c(par[2], par[3]))
  return(fit.epid(log.R=par[1], GT=GT, ...))
}
