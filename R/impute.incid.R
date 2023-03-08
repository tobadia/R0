# Name   : impute.incid
# Desc   : Optimizes an unkown vector to determine best-fitting censored 
#          incidence values for unobserved cases
# Date   : 2012/02/20
# Update : 2023/03/02
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Optimiziation routine for incidence imputation
#' 
#' @description
#' When first records of incidence are unavailable, tries to impute censored 
#' cases to rebuild a longer epidemic vector.
#' 
#' @details
#' This function is not intended for stand-alone use. It optimizes the values 
#' of vect, based upon minimization of deviation between actual epidemics data 
#' and observed generation time. The optimized function is [censored.deviation()], 
#' which returns the deviation used for minimization. Stand-alone use can be 
#' conducted, however this assumes data are all of the correct format. 
#' 
#' @param CD.optim.vect Vector of two elements (`c(multiplicative factor, log(highest imputed data))`) to be optimized.
#' @param CD.epid Original epidemic vector, output of [check.incid()].
#' @param CD.R0 Assumed R0 value for the original epidemic vector.
#' @param CD.GT Generation time distribution to be used for computations.
#' 
#' @return
#' A vector with both imputed incidence and source available data.
#' 
#' @importFrom stats optim
#' 
#' @export
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

impute.incid = function(
    CD.optim.vect, 
    CD.epid, 
    CD.R0, 
    CD.GT 
) 
  
  
  # Code
  
{ 
  optimized.parameters <- optim(CD.optim.vect, censored.deviation, epid=CD.epid, R0=CD.R0, GT=CD.GT)$par
  
  vect <- c(rep(optimized.parameters[1], (length(CD.GT$GT)-1)), optimized.parameters[2])
  censored.incidence <- rev(exp(rev(vect)[1]) * cumprod(c(1, exp(rev(vect)[-1])/(1+exp(rev(vect)[-1])))))
  
  #Bugfix for computed values: set to 1 if optimization returns a value between 0 and 1.
  censored.incidence[censored.incidence < 1] <- 1
  #cat(censored.incidence,"\n")
  
  return(round(c(censored.incidence,CD.epid$incid)))
  
}
