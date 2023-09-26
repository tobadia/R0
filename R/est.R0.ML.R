# Name   : est.R0.ML
# Desc   : Estimation of basic Reproduction Number using Maximum Likelihood method,
#          as presented by White & Pagano
# Date   : 2011/11/09
# Update : 2023/03/01
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Estimate R0 by Maximum Likelihood
#' 
#' @description
#' Estimate R0 by maximum likelihood, assuming Poisson distribution of offsprings
#' generated from infected individuals at each time step.
#' 
#' @note
#' This is the implementation of the method provided by White & Pagano (2009).
#' 
#' @references
#' White, L.F., J. Wallinga, L. Finelli, C. Reed, S. Riley, M. Lipsitch, and M. Pagano. "Estimation of the Reproductive Number and the Serial Interval in Early Phase of the 2009 Influenza A/H1N1 Pandemic in the USA." Influenza and Other Respiratory Viruses 3, no. 6 (2009): 267-276.
#' 
#' @details
#' For internal use. Called by [estimate.R()].
#' 
#' White & Pagano (2009) detail two maximum likelihood methods for estimatig the 
#' reproduction ratio. The principle of the methods described by White & all is to 
#' compute the expected number of cases in the future, and optimise to get R using 
#' a Poisson distribution.
#' 
#' The first (and used by default in this package) assumes that the serial interval 
#' distirbution is known, and subsequently the likelihood is only maximised depending 
#' on the value of R. 
#' 
#' The second method can be used if the serial interval distribution is unknown: in 
#' that case, the generation time is set to follow a Gamma distribution with two 
#' parameters (size, shape), and the optimization routine finds the values of R, size 
#' and shape that maximize the likelihood. However, the epidemic curve must be long 
#' enough to account for a whole generation. The authors showed that this is achieved 
#' when the cumulated amount of incident cases reaches approximately 150. 
#' When using this method, the flag `unknown.GT` must be set to `TRUE`. GT must still 
#' be provided with a `R0.GT`-class object, though its mean and sd will be recycled 
#' as starting value for the optimization routine.
#' 
#' The 95% confidence interval is achieved by profiling the likelihood.
#' 
#' In addition to the estimation method, we implemented a framework to impute 
#' unobserved incidence data, when the epidemic curve is found to not be available
#' from the start of the outbreak. The details of this method are availble in the
#' Supplementary Material S1 from \doi{10.1186/1472-6947-12-147}{Obadia et al., 2012}.
#' 
#' @param epid Object containing epidemic curve data. 
#' @param GT Generation time distribution from [generation.time()]. 
#' @param import Vector of imported cases. 
#' @param t Vector of dates at which incidence was observed. 
#' @param begin At what time estimation begins. 
#' @param end Time at which to end computation. 
#' @param date.first.obs Optional date of first observation, if `t` not specified. 
#' @param time.step Optional. If date of first observation is specified, number of day between each incidence observation. 
#' @param range Range of values over which the MLE must be explored. 
#' @param unknown.GT Boolean value. When GT distribution is unknown, it is estimated jointly (see details). 
#' @param impute.incid Boolean value. If `TRUE`, will impute unobserved cases at the beginning of the epidemic to correct for censored data.
#' @param checked Internal flag used to check whether integrity checks were ran or not.
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' A list with components:
#' \item{R}{The estimate of the reproduction ratio.}
#' \item{conf.int}{The 95% confidence interval for the R estimate.}
#' \item{epid}{Original or augmented epidemic data, depending whether [impute.incid()] is set to `FALSE` or `TRUE`.}
#' \item{epid.orig}{Original epidemic data.}
#' \item{GT}{Generation time distribution uised in the computation.}
#' \item{begin}{Starting date for the fit.}
#' \item{begin.nb}{The number of the first day used in the fit.}
#' \item{end}{The end date for the fit.}
#' \item{end.nb}{The number of the las day used for the fit.}
#' \item{pred}{Prediction on the period used for the fit.}
#' \item{Rsquared}{Correlation coefficient between predicted curve (by [fit.epid()]) and observed epidemic curve.}
#' \item{call}{Call used for the function.}
#' \item{method}{Method used for fitting.}
#' \item{method.code}{Internal code used to designate method.}
#' 
#' @importFrom stats uniroot poisson qchisq optim optimize glm
#' 
#' @export
#'
#' @keywords internal
#' 
#' @example tests/est.R0.ML.R
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

est.R0.ML <- function(
    epid, 
    GT, 
    import         = NULL, 
    t              = NULL, 
    begin          = NULL, 
    end            = NULL, 
    date.first.obs = NULL, 
    time.step      = 1, 
    range          = c(0.01,50), 
    unknown.GT     = FALSE, 
    impute.incid   = FALSE, 
    checked        = FALSE, 
    ... 
) 


# Code

{
  CALL <- match.call()
  # Various class and integrity checks
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
                                   methods = "ML")
    
    begin <- parameters$begin
    end <- parameters$end
  }
  
  epid <- check.incid(epid, t, date.first.obs, time.step)
  begin.nb <- which(epid$t == begin)
  end.nb <- which(epid$t == end)
  
  if (!is.null(import) & length(import) != length(epid$incid)) {
    stop("Import vector and incidence vector do not have the same length.")
  }
  if (is.null(import)) {
    import <- rep(0, length(epid$incid))
  }
  
  #  if (impute.incid == TRUE) {
  #    begin.nb = 1
  #    end.nb = length(epid$incid)
  #  }
  
  # Backup of original epidemic data
  epid.orig <- epid
  
  # Truncate at end if necessary
  epid <- list(incid = epid$incid[begin.nb:end.nb], 
               t = epid$t[begin.nb:end.nb])
  import <- import[begin.nb:end.nb]
  
  # Make a likelihood that can be optimized
  logR <- log(1) # Initial value is taken at 1
  
  # In case of joint estimation with GT distribution, process GT first
  if (unknown.GT) {
    GT.par <- optim(c(logR, GT$mean, GT$sd), fit.epid.optim, epid = epid, import = import, control = list(fnscale=-1))$par
    GT <- generation.time("gamma", c(GT.par[2], GT.par[3]))
  }
  
  # Explore the Poisson log-likelihood for best-fitting R0
  logR.par <- optimize(fit.epid, log(range), GT = GT, epid = epid, import = import, maximum=TRUE)
  
  if ((exp(logR.par$maximum) == range[1]) | (exp(logR.par$maximum) == range[2])) { 
    warning("Algorithm converged to boundary. Try increasing 'range'.")
  }
  
  # Should get the confidence interval by solving likelihood. 
  # Use uniroot starting from current estimate.
  R.max <- uniroot(fit.epid, lower = logR.par$maximum, upper = log(range[2]), offset = logR.par$objective - qchisq(0.95, df = 1), 
                   epid = epid, GT = GT, import = import)
  R.min <- uniroot(fit.epid, lower = log(range[1]), upper = logR.par$maximum, offset = logR.par$objective - qchisq(0.95, df = 1), 
                   epid = epid, GT = GT, import = import)
  
  # Calculate the corresponding Poisson prediction and derive deviance-measured R-squared value
  pred <- fit.epid(log.R = logR.par$maximum, epid = epid, GT = GT, import = import, pred = TRUE)
  mod <- glm(epid$incid ~ pred, family = poisson())
  Rsquared <- (mod$null.deviance - mod$deviance) / (mod$null.deviance)
  
  # If data are censored and need to be imputed, the ML method is called recursively
  # and R is updated after each call. Its trace is kept in R.chain.
  if (impute.incid) {
    # Initialize imputation with starting parameters
    R.chain <- exp(logR.par$maximum)
    full.incid <- impute.incid(CD.optim.vect = c(0,0), 
                               CD.epid = epid.orig, 
                               CD.R0 = R.chain, 
                               CD.GT = GT)
    R.chain <- c(R.chain, est.R0.ML(epid = full.incid, GT = GT, begin = 1, end = as.numeric(end.nb + length(GT$GT)), impute.incid = FALSE)$R)
    
    # Recursively impute censored cases until convergence
    i <- 1
    while (abs(R.chain[i] - R.chain[i-1]) > 0.0001) {
      # When too few values are available for convergence, break condition is needed
      # Most common case is convergence to 2nd digit (which should be enough for any estimation...)
      i <- i + 1
      
      if ((i > 1000) && (abs(R.chain[i-1] - R.chain[i-2]) < 0.01)) {
        break
        warning("Algorithm didn't converge to lower than 1e-4 after 1000 iterations. However, termination was achieved with a 1e-2 threshold over the last two values.")
      }
      
      full.incid <- impute.incid(c(0,0), epid.orig, R.chain[i], GT)
      R.full.incid <- est.R0.ML(epid = full.incid, GT = GT, begin = 1, end = as.numeric(end.nb + length(GT$GT)), impute.incid = FALSE)
      R.chain <- rbind(R.chain, R.full.incid$R)
      R.conf.int <- R.full.incid$conf.int
      pred.full.incid <- R.full.incid$pred
    }
    
    full.epid <- check.incid(incid = full.incid, time.step = time.step, date.first.obs = (epid$t[1] - length(GT$GT)))
  }
  
  if (!impute.incid) {
    return (structure(list(R = exp(logR.par$maximum), 
                           conf.int = c(exp(R.min$root), exp(R.max$root)), 
                           epid = epid.orig, 
                           GT = GT, 
                           begin = begin, 
                           begin.nb = begin.nb, 
                           end = end, 
                           end.nb = end.nb, 
                           pred = pred, 
                           Rsquared = Rsquared, 
                           call = CALL, 
                           method = "Maximum Likelihood", 
                           method.code = "ML"),
                      class = "R0.R"))
  }
  else {
    return(structure(list(R = R.chain[length(R.chain)], 
                          conf.int = R.conf.int, 
                          epid = full.epid, 
                          epid.orig = epid.orig, 
                          GT = GT, 
                          begin = full.epid$t[1], 
                          begin.nb = 1, 
                          end = end, 
                          end.nb = which(full.epid$t == end), 
                          pred = pred.full.incid, 
                          Rsquared = Rsquared, 
                          call = CALL, 
                          method = "Maximum Likelihood", 
                          method.code = "ML"),
                     class = "R0.R"))
  }
  
}
