# Name   : est.R0.EG
# Desc   : Estimation of basic Reproduction Number using Exponential Growth method,
#          as presented by Wallinga & Lipsitch
# Date   : 2011/11/09
# Update : 2023/03/01
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Estimate R0 from exponential growth rate of an epidemic
#' 
#' @description
#' Estimate R0 from the initial phase of an epidemic when incident cases are 
#' presumed to follow an exponential distribution.
#' 
#' @note
#' This is the implementation of the method provided by Wallinga & Lipsitch (2007).
#' 
#' @references
#' Wallinga, J., and M. Lipsitch. "How Generation Intervals Shape the Relationship Between Growth Rates and Reproductive Numbers." Proceedings of the Royal Society B: Biological Sciences 274, no. 1609 (2007): 599.
#' 
#' @details
#' For internal use. Called by [estimate.R()].
#' 
#' Method "poisson" uses Poisson regression of incidence to estimate the exponential 
#' growth rate. Method "linear" uses linear regression of log(incidence) against time.
#' 
#' The 95% confidence interval is computed from the 1/M(-r) formula, using bounds 
#' on r from the Poisson or linear regression.
#' 
#' @param epid Object containing epidemic curve data.
#' @param GT Generation time distribution from [generation.time()].
#' @param t Vector of dates at which incidence was observed.
#' @param begin At what time estimation begins.
#' @param end Time at which to end computation.
#' @param date.first.obs Optional date of first observation, if `t` not specified
#' @param time.step Optional. If date of first observation is specified, number of day between each incidence observation
#' @param reg.met Regression method used. Default is "poisson" (for GLM), but can be forced to "linear".
#' @param checked Internal flag used to check whether integrity checks were ran or not.
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' A list with components:
#' \item{R}{The estimate of the reproduction ratio.}
#' \item{conf.int}{The 95% confidence interval for the R estimate.}
#' \item{r}{Exponential growth rate of the epidemic.}
#' \item{conf.int.r}{Confidence interval of the exponential growth rate of the epidemic.}
#' \item{Rsquared}{The deviance R-squared measure for the considered dates and model.}
#' \item{epid}{Original epidemic data.}
#' \item{GT}{Generation time distribution used in the computation.}
#' \item{data.name}{Name of the data used in the fit.}
#' \item{begin}{Starting date for the fit.}
#' \item{begin.nb}{The number of the first day used in the fit.}
#' \item{end}{The end date for the fit.}
#' \item{end.nb}{The number of the las day used for the fit.}
#' \item{fit}{Method used for fitting.}
#' \item{pred}{Prediction on the period used for the fit.}
#' \item{method}{Method for estimation.}
#' \item{method.code}{Internal code used to designate method.}
#' 
#' @importFrom stats confint poisson predict lm glm coefficients
#' 
#' @export
#'
#' @keywords internal
#' 
#' @example tests/est.R0.EG.R
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

est.R0.EG <-function( 
    epid, 
    GT, 
    t              = NULL, 
    begin          = NULL, 
    end            = NULL, 
    date.first.obs = NULL, 
    time.step      = 1, 
    reg.met        = "poisson", 
    checked        = FALSE,  
    ...
)


# Code

{
  DNAME <-  deparse(substitute(epid))
  #CALL = sapply(match.call()[-1], deparse)
  CALL <- match.call()
  
  # Various class and integrity check
  if (!checked) {
    parameters <- integrity.checks(epid = epid, 
      GT = GT, 
      t = t, 
      begin = begin, 
      end = end, 
      date.first.obs = date.first.obs, 
      time.step = time.step, 
      AR = NULL, 
      S0 = NULL, 
      methods = "EG")

    begin <- parameters$begin
    end <- parameters$end
  }

  epid <- check.incid(epid, t, date.first.obs, time.step)
  begin.nb <- which(epid$t == begin)
  end.nb <- which(epid$t == end)
  
  
  # Backup original epidemic data
  epid.orig <- epid
  
  # Epidemic data used for fit is truncted to keep only values within [begin,end]
  #epid <- list(incid = epid$incid[begin.nb:end.nb], t = epid$t[begin.nb:end.nb], t.glm = seq(from=begin.nb, to=end.nb, by=1))
  #epid <- list(incid = epid$incid[begin.nb:end.nb], t = epid$t[begin.nb:end.nb], t.glm = epid$t[begin.nb:end.nb])
  epid <- list(incid = epid$incid[begin.nb:end.nb], t = epid$t[begin.nb:end.nb])
  
  # Different methods to estimate epidemic growth rate (r) from data
  # Method 1: Poisson regression
  if (reg.met == "poisson") {
    # Fit Poisson regression
    #mod <- glm(incid ~ t.glm, family = poisson(), data = epid)
    mod <- glm(incid ~ t, family = poisson(), data = epid)
    
    # Get relevant data from mod
    Rsquared <- (mod$null.deviance - mod$deviance) / (mod$null.deviance)
    r <- coefficients(mod)[2]
    conf.int <- confint(mod)[2, ]
    pred <- predict(mod, type = "response")
  }
  
  # Method 2: Linear regression
  else if (reg.met == "linear") {
    # Fit log-linear regression
    #mod <- lm((log(incid)) ~ t.glm, data = epid)
    mod <- lm((log(incid)) ~ t, data = epid)
    
    # Get relevant data from mod
    Rsquared <- summary(mod)$r.squared
    r <- coefficients(mod)[2]
    conf.int <- confint(mod)[2, ]
    pred <- exp(predict(mod, type = "response"))
  } 
  
  # Apply method of Wallinga / Lipsitch for discretized Laplace transform
  R <- as.numeric(R.from.r(r, GT))
  R.inf <- as.numeric(R.from.r(conf.int[1], GT))
  R.sup <- as.numeric(R.from.r(conf.int[2], GT))
  
  return(structure(list(R = R, 
    conf.int = c(R.inf, R.sup), 
    r = r, 
    conf.int.r = conf.int, 
    Rsquared = Rsquared, 
    epid = epid.orig, 
    GT = GT, 
    data.name = DNAME, 
    call = CALL, 
    begin = begin, 
    begin.nb = begin.nb, 
    end = end, 
    end.nb = end.nb, 
    method = "Exponential Growth", 
    pred = pred, 
    fit = reg.met, 
    method.code = "EG"), 
  class = "R0.R"))
}
