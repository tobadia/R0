# Name   : est.R0.SB
# Desc   : Estimation of Reproduction Number distribution using previous R(t)
#          distribution as a Bayesian prior, as presented by Bettencourt & Ribeiro
# Date   : 2011/11/09
# Update : 2023/03/01
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Estimate the time dependent R using a Bayesian method
#' 
#' @description
#' Estimate R by a sequential Bayesian method, using known data up to a point 
#' in time as a Bayesian prior for the next iteration (see details).
#' 
#' @note
#' This is the implementation of the method provided by Bettencourt & Ribeiro (2008).
#' 
#' @references
#' Bettencourt, L.M.A., and R.M. Ribeiro. "Real Time Bayesian Estimation of the Epidemic Potential of Emerging Infectious Diseases." PLoS One 3, no. 5 (2008): e2185.
#' 
#' @details
#' For internal use. Called by [estimate.R()].
#' 
#' Initial prior is an unbiased uniform distribution for R, between 0 and the maximum 
#' of incid(t+1) - incid(t). For each subsequent iteration, a new distribution is 
#' computed for R, using the previous output as new prior.
#' 
#' The 95% confidence intervan is achieved by a cumulative sum of the posterior 
#' distirbution of R and corresponds to the 2.5-th and 97.5-th percentiles.
#' 
#' @param epid Object containing epidemic curve data. 
#' @param GT Generation time distribution from [generation.time()]. 
#' @param t Vector of dates at which incidence was observed. 
#' @param begin At what time estimation begins (unused by this method, just there for plotting purposes).
#' @param end At what time estimation ends (unused by this method, just there for plotting purposes).
#' @param date.first.obs Optional date of first observation, if `t` not specified. 
#' @param time.step Optional. If date of first observation is specified, number of day between each incidence observation. 
#' @param force.prior Set to any custom value to force the initial prior as a uniform distribution on [0 ; value].
#' @param checked Internal flag used to check whether integrity checks were ran or not.
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' A list with components:
#' \item{R}{vector of R values.}
#' \item{conf.int}{95% confidence interval for estimates.}
#' \item{proba.Rt}{A list with successive distribution for R throughout the outbreak.}
#' \item{GT}{Generation time distribution used in the computation.}
#' \item{epid}{Original epidemic data.}
#' \item{begin}{Begin date for the fit.}
#' \item{begin.nb}{Index of begin date for the fit.}
#' \item{end}{End date for the fit.}
#' \item{end.nb}{Index of end date for the fit.}
#' \item{pred}{Predictive curve based on most-likely R value.}
#' \item{data.name}{Name of the data used in the fit.}
#' \item{call}{Complete call used to generate results.}
#' \item{method}{Method for estimation.}
#' \item{method.code}{Internal code used to designate method.}
#' 
#' @export
#'
#' @keywords internal
#' 
#' @example tests/est.R0.SB.R
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

est.R0.SB <- function(
    epid, 
    GT, 
    t              = NULL, 
    begin.         = NULL, 
    end            = NULL, 
    date.first.obs = NULL, 
    time.step      = 1, 
    force.prior    = FALSE, 
    checked        = FALSE, 
    ... 
)


# Code

{
  #We should think of imports and how to deal with this.
  DNAME <-  deparse(substitute(epid))
  CALL <- match.call()
  
  #Various class and integrity checks
  if (checked == FALSE) {
    parameters <- integrity.checks(epid, t, GT, begin, end, date.first.obs, time.step, AR=NULL, S0=NULL, methods="SB")
    begin <- parameters$begin
    end <- parameters$end
  }
  epid <- check.incid(epid, t, date.first.obs, time.step)
  epid.orig <- epid
  begin.nb <- which(epid$t == begin)
  end.nb <- which(epid$t == end)
  epid$incid <- epid$incid[begin.nb:end.nb]
  epid$t <- epid$t[begin.nb:end.nb]
  time.step <- as.numeric(epid$t[2]-epid$t[1])
  
  ## Beginning of estimation 
  
  Tmax <- length(epid$incid)
  gamma <- 1/GT$mean # 1/gamma is the infectious period as defined in a standard SIR model
  proba.Rt <- list() #list to store successive densities.
  
  
  ## Initial distribution of Rt
  
  if (force.prior == FALSE) {
    #For the first iteration, we admit Rt to be following a unfiorm distribution
    #on [0 ; maximum.delta.incid]
    delta.incid <- epid$incid[2:length(epid$incid)] - epid$incid[1:(length(epid$incid)-1)]
    maximum.delta.incid <- which.max(delta.incid)
    maximum.delta.incid <- epid$incid[maximum.delta.incid+1] - epid$incid[maximum.delta.incid]
  }
  else {
    #Prior by Bettencourt & Ribeiro is an unbiased uniform distribution on [0;3]
    #This one is added for comparison purpose.
    maximum.delta.incid <- as.numeric(force.prior)
  }
  
  proba.Rt[[1]] <- rep(1/(maximum.delta.incid*100 + 1), (maximum.delta.incid*100 + 1))
  Rt.range <- seq(0,maximum.delta.incid, .01)
  most.likely.Rt <- rep(0, length(epid$incid))
  
  #Useful results will be stored in a data.frame for easy preview
  Rt.quant <- matrix(NA, length(epid$incid), ncol=4)
  colnames(Rt.quant) <- c("Date","R(t)", "CI[lower]", "CI[upper]")
  
  
  ## Loop on epidemic duration
  for (s in 2:Tmax) {
    sequence.factorielle <- 1
    if (epid$incid[s] != 0) {
      sequence.factorielle <- seq(from=1, to=epid$incid[s], by=1)
    }
    tmp <- epid$incid[s]*(log(epid$incid[s-1]) + time.step*gamma*(Rt.range - 1)) - epid$incid[s-1]*exp(time.step*gamma*(Rt.range - 1)) - sum(log(sequence.factorielle))
    
    tmp.proba.Rt <- exp(log(proba.Rt[[s-1]]) + tmp)
    #Break if incidence data starts producing NaN values
    if ((sum(tmp.proba.Rt) == 0) | NaN %in% (sum(tmp.proba.Rt))) {
      end.nb <- s-1
      end <- epid$t[end.nb]
      break
    }
    tmp.proba.Rt <- tmp.proba.Rt/sum(tmp.proba.Rt)
    
    proba.Rt[[s]] <- tmp.proba.Rt
    
    # Extracting most likely Rt value.
    most.likely.Rt[s] <- Rt.range[which.max(proba.Rt[[s]])]
    
    #Computing Confidence Interval around most plausible value
    Rt.quant[s-1,1] <- epid$t[s-1]
    Rt.quant[s-1,2] <- most.likely.Rt[s]
    cumsum.proba <- cumsum(proba.Rt[[s]])
    Rt.quant[s-1,3] <- Rt.range[which.max((cumsum(proba.Rt[[s]])) >= 0.025)]
    Rt.quant[s-1,4] <- Rt.range[which.max((cumsum(proba.Rt[[s]])) >= 0.975)]
    
  }
  
  #Last line of Rt.quant is set to 0 for graphical purposes
  #Rt.quant[length(epid$t),1] = epid$t[length(epid$t)]
  #Rt.quant[length(epid$t),2] = Rt.quant[length(epid$t)-1,2]
  #Rt.quant[length(epid$t),3] = Rt.quant[length(epid$t)-1,3]
  #Rt.quant[length(epid$t),4] = Rt.quant[length(epid$t)-1,4]
  Rt.quant <- data.frame(na.omit(Rt.quant))
  
  #Fix for incorrect storage format in matrix
  if (!is.numeric(epid$t)) {
    Rt.quant$Date <- as.Date(Rt.quant$Date, origin=(as.Date("1970-01-01")))
  }
  
  #Predictive curve is also reported from successive posterior distributions
  pred <- c(epid$incid[1], rep(NA,(end.nb-begin.nb)))
  
  #Based on the most-likely R value at the end of the simulation, will compute the predicted exponential growth curve
  for (t in 1:(Tmax-1)) {
    pred[t+1] <- pred[t]*exp(gamma*time.step*(Rt.quant$R.t[t]-1))
  }
  
  R <- Rt.quant[1:(end.nb-begin.nb),2]
  conf.int <- Rt.quant[1:(end.nb-begin.nb),3:4]
  rownames(conf.int) <- as.character(epid$t[1:(end.nb-begin.nb)])
  return(structure(list(R=R, conf.int=conf.int, proba.Rt=proba.Rt, GT=GT, epid=epid.orig, begin=begin, begin.nb=begin.nb, end=end, end.nb=end.nb, pred=pred, data.name=DNAME, call=CALL, method="Sequential Bayesian", method.code="SB"),class="R0.R"))    #return everything
  
}
