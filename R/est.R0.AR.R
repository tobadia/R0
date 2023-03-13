# Name   : est.R0.AR
# Desc   : Estimation of basic Reproduction Number using Attack Rate method 
#          (derived from SIR model), as presented by Dietz.
# Date   : 2011/11/09
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Estimate R0 from attack rate of an epidemic
#' 
#' @description
#' Estimate basic reproduction number R0 from Attack Rate (AR) of an epidemic.
#' 
#' @note
#' This is the implementation of the formula by Dietz (1993).
#' 
#' @references
#' Dietz, K. "The Estimation of the Basic Reproduction Number for Infectious Diseases." Statistical Methods in Medical Research 2, no. 1 (March 1, 1993): 23-41.
#' 
#' @details
#' For internal use. Called by [estimate.R()]. 
#' 
#' In the simple SIR model, the relation between R0 and the Attack Rate is in the 
#' form \eqn{R0 = -ln((1-AR)/S0) / (AR - (1-S0))}. If the population size is provided, 
#' the variance of R0 is estimated using the delta method. The hypothesis are that 
#' of homogeneous mixing, no more transmission (epidemic ended), no change in 
#' transmission or interventions during the epidemic. This estimate may be correct 
#' in closed populations, and may be less valid in other cases.
#' 
#' The correction for incomplete susceptibility is based on the SIR model equations.
#' 
#' A 95% confidence interval is computed for the attack rate considering the total
#' population size (\eqn{CI(AR) = AR +/- 1.96*sqrt(AR*(1-AR)/n)}), and so the 
#' confidence interval for the reproduction number is computed with these extreme values.
#' 
#' @param AR Attack rate as a percentage from total population.
#' @param incid Sum of incident cases, possibly in the form of a vector of counts.
#' @param pop.size Population size in which the incident cases were observed.
#' @param S0 Initial proportion of the population considered susceptible.
#' @param checked Internal flag used to check whether integrity checks were ran or not.
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' A list with components:
#' \item{epid}{The vector of incidence, after being correctly formated by [check.incid()]. Used only by [plotfit()]}
#' \item{R}{The estimate of the reproduction ratio.}
#' \item{conf.int}{The 95% confidence interval for the R estimate.}
#' \item{AR}{Original attack rate.}
#' \item{begin.nb}{First date of incidence record. Used only by [plotfit()].}
#' \item{end.nb}{Last date of incidence record. Used only by [plotfit()].}
#' \item{method}{Method used for the estimation.}
#' \item{method.code}{Internal code used to designate method.}
#' 
#' @export
#'
#' @keywords internal
#' 
#' @example tests/est.R0.AR.R
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

est.R0.AR <- function(
    AR       = NULL, 
    incid    = NULL, 
    pop.size = NULL, 
    S0       = 1, 
    checked  = FALSE, 
    ... 
) 
  
  
  # Code
  
{
  # Various class and integrity checks
  if (!checked) {
    integrity.checks(epid = epid, 
                     GT = NULL, 
                     t = t, 
                     begin = NULL, 
                     end = NULL, 
                     date.first.obs = NULL, 
                     time.step = NULL, 
                     AR = AR, 
                     S0 = S0, 
                     methods = "AR")
  }
  
  if (!is.null(incid)) {
    epid <- check.incid(incid)
  }
  else {
    epid <- NULL
  }
  
  # Required : either (AR, incidence) or (AR, pop.size) to start simulation
  if (is.null(AR) & any(c(is.null(incid), is.null(pop.size)))) {
    stop("Either 'AR' alone or both 'AR / incid' and 'pop.size' must be provided")
  }
  
  # 1- AR is not provided: it's computed as sum(incid)/pop.size
  if (is.null(AR)) {
    # When incid provided as a series of incident cases, sum it
    if (length(incid) > 1) {
      incid <- sum(incid)
    }
    
    
    if (any(c(incid, pop.size) <= 0 )) {
      stop(paste("'incid' =", incid, " and 'pop.size' =", pop.size, "must be nonnegative"))
    }
    
    if (pop.size < incid) {
      stop(paste("'pop.size' =", pop.size, "must be greater than 'incid'=", incid))
    }
    
    # Actual AR is now computed
    AR <- incid/pop.size
  }
  
  # 2- AR is provided
  else {
    # Obviously AR is between 0 and 1
    if (AR <= 0 | AR >= 1) {
      stop(paste("'AR' =", AR, "must be between 0 and 1"))
    }
    
    if (is.null(pop.size)) {
      pop.size <- NA
    }
  }
  
  # R0 is derived from Attack Rate based on SIR model (see Dietz)
  R0.from.AR <- function(AR, S0) {-log((1 - AR) / S0) / (AR - (1 - S0))}
  
  R0 <- R0.from.AR(AR = AR, S0 = S0)
  CI95 <- c(R0.from.AR(AR - 1.96 * sqrt(AR * (1 - AR) / pop.size), S0), 
            R0.from.AR(AR + 1.96 * sqrt(AR * (1 - AR) / pop.size), S0))
  
  # Variance of R0 is estimated using Delta method
  var.R0 <- ((-((-1 + AR + S0) / (-1 + AR)) + log((1 - AR) / S0)) / (-1 + AR + S0)^2) * AR * (1-AR) / pop.size
  
  return(structure(list(epid = epid, 
                        R = R0, 
                        var = var.R0, 
                        conf.int = CI95, 
                        begin.nb = 1, 
                        end.nb = length(incid), 
                        AR = AR, 
                        method = "Attack Rate", 
                        method.code = "AR"), 
                   class = "R0.R"))
}
