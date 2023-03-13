# Name   : censored.deviation
# Desc   : Computes the difference between theoretical incidence (from 
#          Ni = R0*sum(Nj*w(t-j))) and observed data. The squared-sum of this
#          difference is returned, for the first incidence values.
# Date   : 2012/02/20
# Update : 2023/02/28
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Deviation between theorietical incidence and observed data.
#'
#' @description
#' When first records of incidence are unavailable, tries to impute censored 
#' cases to rebuild longer epidemic vector.
#' 
#' @details
#' For internal use. Called by [impute.incid()].
#' 
#' This function is not intended for stand-alone use. It computes the difference 
#' between theoretical incidence and observed epidemics, for a given vector of 
#' initial values. To find the find best-fitting incidence values, this same 
#' vector must be optimized, to minimize the value returned by SCE.
#' 
#' @param optim.vect Vector of two elements `c(multiplicative factor, log(highest imputed data))` corresponding to initial values.
#' @param epid Original epidemic vector, output of [check.incid()].
#' @param R0 Assumed R0 value for the original epidemic vector
#' @param GT Generation time distribution to be used for computations
#'
#' @return
#' The deviation between \enq{E(N_{t})} and \eqn{N_{t}} : sum((E(Nt) - Nt)^2).
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

censored.deviation <- function(
    optim.vect, 
    epid, 
    R0, 
    GT 
) 
  
  
  # Code
  
{
  # Do not proceed unless GT is of the right class
  if (!inherits(GT, "R0.GT")) {
    stop("GT object must be of class R0.GT.")
  }
  GT <- GT$GT
  
  # optim.vect is of form c(multicative.factor, log(first.imputed.data))
  # This allows for computing ONLY increasing values for imputed data, and
  # prevents optim() from finding negative/irrelevant results
  if (length(optim.vect) != 2) {
    stop("optim.vect must have only two values.")
  }
  
  if (length(epid$incid) < length(GT)) {
    stop("Not enough incidence values were provided to run the simulation.")
  }
  
  # vect is a temporary vector, used to ventilate theoretical incidence 
  # over a given GT distribution. 
  vect <- c(rep(optim.vect[1], (length(GT) - 1)), 
            optim.vect[2])
  
  # censored.incidence is the actual vector of theoretical imputed incidence
  # rev(vect)[1] is first imputed value
  # rev(vect)[-1] is the series of multiplicative values propagated above in vect
  censored.incidence <- rev(exp(rev(vect)[1]) * cumprod(c(1, exp(rev(vect)[-1]) / (1 + exp(rev(vect)[-1])))))
  
  #cat(censored.incidence)
  
  #val is the deviation between theory and observations.
  val <- 0
  for (t in length(GT):2) {
    # print(c(epid$incid[(t-1):1],exp(vect[length(GT):t])))
    val <- val + (epid$incid[t-1] - R0 * sum(c(epid$incid[(t-1):1], censored.incidence[length(GT):t]) * GT))^2
    
    # Use for debug:
    #print(paste("t:",t," N : ", epid$incid[t-1], "EN :",  R0 * sum(c(epid$incid[(t-1):1],exp(vect[length(GT):t])) * GT)))
    #cat(epid$incid[t-1]," ", R0 * sum(c(epid$incid[(t-1):1],censored.incidence[length(GT):t]) * GT),"\n")
  }
  
  #cat(val,"\n")
  #cat(censored.incidence,"\n")
  return(val)
  
}
