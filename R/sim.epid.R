# Name   : sim.epid
# Desc   : Simulate epidemic outbreaks of specified R0 and generation time 
#          distribution
# Date   : 2012/04/11
# Update : 2023/03/03
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Epidemic outbreak simulation
#' 
#' @description
#' Generates several epidemic curves with specified distribution and reproduction 
#' number.
#' 
#' @details
#' This function is only used for simulation purposes. 
#' The output is a matrix of n columns (number of outbreaks) by m rows (maximum 
#' length of an outbreak).
#' 
#' When using rnbinom with `mean` and `size` moments, the variance is given by 
#' `mean + mean^2/size` (see [stats::rnbinom()]). One should determine the size 
#' accordingly to the R0 value to increase the dispersion. From the previous 
#' formula for the variance, \eqn{Var(X) = k*R_{0}} implies that 
#' \eqn{size = R0/(k-1)}.
#' 
#' @param epid.nb Number of epidemics to be simulated (defaults to 1)
#' @param GT Generation time distribution from [generation.time()]. 
#' @param R0 Basic reproduction number, in its core definition.
#' @param epid.length Maximum length of the epidemic (cases infected after this length will be truncated).
#' @param family Distribution of offspring. Can be either `"poisson"` (default) or `"negbin"`.
#' @param negbin.size If family is set to "negbin", sets the size parameter of the negative binomial distribution.
#' @param peak.value Threashold value for incidence before epidemics begins decreasing
#' 
#' @return
#' A matrix with epidemics stored as columns (incidence count).
#' 
#' @export
#' 
#' @example tests/sim.epid.R
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

sim.epid <- function(
    epid.nb, 
    GT, 
    R0, 
    epid.length, 
    family, 
    negbin.size = NULL, 
    peak.value  = 50 
)
  
  
  # Code
  
{
  
  #Various content integrity checks
  if (!inherits(GT, "R0.GT")) {
    stop("GT object must be of class R0.GT.")
  }
  
  #If negbin.size is omitted, size parameter is set so that variance = 10*R0. See details.
  if (family=="negbin" & is.null(negbin.size)) {
    negbin.size <- R0/4
  }
  GT <- GT$GT
  #R0.orig <- R0
  
  #Each epidemic is stored as a matrix column
  epidemics <- matrix(data=0, nrow=epid.length, ncol=epid.nb)
  
  #Loop for the required number of epidemic curves
  for (n in 1:epid.nb) {
    
    #Vector of cases with one index case
    sim.epid = c(1,rep(0,epid.length-1))
    
    #Loop on epidemic duration
    for(t in 1:epid.length) { 
      
      #New cases depend on family type
      if (family=="poisson") {
        new <- rpois(sim.epid[t], R0)
      }
      else if (family=="negbin") {
        new <- rnbinom(sim.epid[t], size=negbin.size, mu=R0)
      }
      newd <-  rmultinom(1, sum(new), GT)[,1]
      sim.epid[t:(t+length(GT)-1)] <- sim.epid[t:(t+length(GT)-1)]+newd
      
      # Threshold so that epidemics eventualy dies out
      if (sim.epid[t+1] > peak.value & t < (epid.length-1)) {
        
        #Changing the R value is like implementing control measure. Uncomment if want to doing so
        #R0 <- 0.7
        
        #One the other hand, we can just stop the epidemic and start another one, as we are only interested in the growth period
        sim.epid[(t+2):epid.length] <- 0
        break
      }
    }
    sim.epid <- sim.epid[!is.na(sim.epid)]
    epidemics[,n] <- sim.epid
    #R0 <- R0.orig
  }
  
  return(epidemics)
}
