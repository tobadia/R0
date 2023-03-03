# Name   : sa.GT
# Desc   : Finds the possible R0 values with supported estimation methods for
#          varying GT parameters
# Date   : 2011/11/09
# Update : 2023/03/03
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Sensitivity of R0 to varying generation time distributions
#' 
#' @description
#' Sensitivity analysis to estimate the variation of reproduction numbers 
#' according to the disitrbution of generation time.
#' 
#' @details
#' By using different Generation Time (GT) distribution, different estimates of 
#' the reproduction ratio can be analyzed.
#' 
#' 
#' @param incid A vector of incident cases.
#' @param GT.type Type of distribution for GT (see GT.R for details).
#' @param GT.mean.range Range of mean values used for all GT distributions throughout the simulation. Must be provided as a vector.
#' @param GT.sd.range Range of standard deviation values used for GT distributions. Must be provided as a vector.
#' @param begin Vector of begin dates for the estimation of epidemic.
#' @param end Vector of end dates for estimation of the epidemic.
#' @param est.method Estimation method used for sensitivity analysis.
#' @param t Dates vector to be passed to estimation function.
#' @param date.first.obs Optional date of first observation, if t not specified.
#' @param time.step Optional. If date of first observation is specified, number of day between each incidence observation.
#' @param ... Parameters passed to inner functions
#' 
#' @return
#'  A data.frame `s.a` with following components :
#'  \item{GT.type}{Type of distribution for GT.}
#'  \item{GT.mean}{Range of means used for tested GTs.}
#'  \item{GT.sd}{Range of standard deviations used for tested GTs.}
#'  \item{R}{Computed value for Reproduction Number given GT.type, GT.mean and GT.sd.}
#'  \item{CI.lowe}{The lower limit of 95% CI for R.}
#'  \item{CI.upper}{The upper limit of 95% CI for R.}
#' 
#' @export
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

sa.GT <- function(
    incid, 
    GT.type, 
    GT.mean.range, 
    GT.sd.range, 
    begin          = NULL, 
    end            = NULL, 
    est.method, 
    t              = NULL, 
    date.first.obs = NULL, 
    time.step      = 1, 
    ... 
)


# Code

{
  #Creating list of GTs and results
  list.GT <- list()
  res <- list()
  
  #Is estimation method supported?
  if ((est.method %in% c("EG", "ML")) == FALSE) {
    stop("Argument 'est.method' should be any of 'EG' or 'ML' to produce results.")
  }
  
  count = 1
  for (i in 1:length(GT.mean.range)) {
    for (j in 1:length(GT.sd.range)) {
      list.GT[[count]] <- generation.time(GT.type, c(GT.mean.range[[i]], GT.sd.range[j]))
      count <- count+1
    }
  }
  
  #tmp.epid is only used to keep trace of (incidence,date) pairs
  tmp.epid <- check.incid(incid, t, date.first.obs, time.step)
  
  #All results will be stored in a matrix
  s.a <- matrix(NA, nrow=length(list.GT), ncol=6)
  
  #Columns are named so that display is easy to read
  colnames(s.a) <- c("GT.Type", "GT.Mean", "GT.SD", "R", "CI.lower", "CI.upper")
  
  for(i in 1:length(list.GT)) {
    #Simulation is ran according to the requested method, with each correct begin/end value
    res <- estimate.R(incid, list.GT[[i]], begin=begin, end=end, t=t, date.first.obs=date.first.obs, time.step=time.step, methods=est.method,...)
    
    
    s.a[i,1] <- GT.type
    s.a[i,2] <- list.GT[[i]]$mean
    s.a[i,3] <- list.GT[[i]]$sd
    if (est.method == "EG") {
      s.a[i,4] <- res$estimates$EG$R
      s.a[i,5] <- res$estimates$EG$conf.int[1]
      s.a[i,6] <- res$estimates$EG$conf.int[2]
    }
    else if (est.method == "ML") {
      s.a[i,4] <- res$estimates$ML$R
      s.a[i,5] <- res$estimates$ML$conf.int[1]
      s.a[i,6] <- res$estimates$ML$conf.int[2]
    }
    
  }
  
  #Return results
  return(s.a=s.a)
  
}
