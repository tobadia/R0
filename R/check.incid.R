# Name   : check.incid
# Desc   : Convert primary input as an array with incidence and related date
# Date   : 2011/11/09
# Update : 2023/03/01
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Check incidence vector in the input
#' 
#' @description
#' Checks incid in the input. For internal use only.
#' 
#' @details
#' For internal use. Called by estimation methods to format incidence input.
#' 
#' `check.incid()` handles everything related to incidence content integrity. 
#' It is designed to generate an output which comply with estimation functions 
#' requirements. Epidemic data can be provided as an epitools object (see below), 
#' or as vectors (incidence, dates, or both).
#' 
#' When dates are provided, they can be in a separate `t` vector, or computed with 
#' the first value and a time step. In the end, the function returns a list with 
#' components`epid` and `t` values. If you plan on using estimation functions on 
#' their own (and not throught [estimate.R()]), be aware that any incorrect input 
#' format will result in erratic behavior and/or crash.
#' 
#' Object incid is either a `list` or `data.frame`. Expect items/columns `$dates` 
#' and/or `$stratum3`. This is expected to work with objects created by the epitools 
#' package (tested with v0.5-6).
#'
#' [epitools::epicurve.dates()] returns (among other things) a list with $dates object. 
#' This list gives incidence per day. Other epicurve methods return $dates along with a 
#' `$<time_period>` object and a `$stratum3`, which contains respectively daily 
#' incidence data agregated by the given time period, and the same data with colnames 
#' that comply with R standard time notation.
#'
#' E.g.: [epitools::epicurve.weeks()] returns $dates, $weeks and $stratum3. $stratum3 
#' object is a list of dates (correct syntax), where each date is repeated to reflect 
#' the incidence value at this time.
#' 
#' Incidence data should not contain negative or missing values. Incidence data and 
#' time vector should have the same length. 
#' 
#' @param incid An object (vector, data.frame, list) storing incidence.
#' @param t Optional vector of dates.
#' @param date.first.obs Optional date of first observation, if t not specified.
#' @param time.step Optional. If date of first observation is specified, number of day between each incidence observation.
#' 
#' @return
#' A list with components `incid` and `t`.
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @example tests/check.incid.R
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

check.incid <- function(
    incid, 
    t              = NULL, 
    date.first.obs = NULL, 
    time.step      = 1 
)
  
  
  # Code
  
{
  # Various class and integrity checks
  if (is.numeric(incid) | is.character(incid) | inherits(incid, "Date")) {
    
    # 1- Incidence provided by date of symptoms onset for each patient
    if (is.character(incid) | inherits(incid, "Date")) {
      # Try to convert incid to dates using standard formats "%Y-%m-%d" / "%Y/%m/%d"
      incid_as_dates <- as.Date(incid)
      
      # If passed, incid was a vector of dates.
      # Check that no dates are closer together than time.step
      if (any(as.numeric(diff(sort(unique(incid_as_dates)))) < time.step)) {
        stop(paste("Dates must all be at least time.step =", time.step, "units apart from each other."))
      }
      
      # Convert to factor, making sure that levels correspond to min - max by time-step
      t <- as.character(seq(from = min(incid_as_dates, na.rm = TRUE), 
                            to = max(incid_as_dates, na.rm  = TRUE, 
                                     by = time.step)))
      incid <- as.numeric(table(factor(incid, levels = t)))
    }
    
    # 1A- When t is (still) NULL so must be inferred from other arguments
    # Check for possible names in incid, or revert to a sequence of integers as last resort.
    # The easiest is in case date.first.obs and time.step have been provided.
    if (is.null(t)) {
      if (is.null(date.first.obs)) {
        if (!is.null(names(incid))) {
          t <- names(incid)
        }
        else {
          t <- seq(from = 1, 
                   to = time.step * (length(incid) - 1) + 1, 
                   by = time.step)
        }
      }
      else {
        t <- seq(from = as.Date(date.first.obs), 
                 to = as.Date(date.first.obs) + time.step * (length(incid) - 1) + 1, 
                 by = time.step)
      }
    }
    
    # When t is not null, numeric or Dates can be treated similarly 
    # 1B- Numeric
    if ((!any(is.na(suppressWarnings(as.numeric(t)))) & !inherits(t, "Date")) | 
        (suppressWarnings((!is.na(strptime(t[1], format="%Y-%m-%d"))) | (!is.na(strptime(t[1], format="%Y/%m/%d")))))) {
      if (inherits(t, "Date")) {
        t <- as.Date(t)
      }
      else {
        t <- as.numeric(t)
      }
      incid <- incid[order(t)]
      t <- t[order(t)]
      
      if (any(duplicated(t))) {
        stop("Duplicated values in t or names in named incid vector.")
      }
      # Check that all t values are present with at least time.step interval
      if (any(diff(t) < time.step)) {
        stop(paste("Values for t must all be at least time.step =", time.step, "units apart from each other."))
      }
      
      epid <- merge(data.frame(t = t, 
                               incid = incid), 
                    data.frame(t = seq(from = min(t, na.rm = TRUE), to = max(t, na.rm = TRUE), by = time.step)), 
                    all.y = TRUE)
      
      epid$incid[is.na(epid$incid)] <- 0
      if (inherits(t, "Date")) {
        t <- as.Date(as.character(epid$t))
      }
      else {
        t <- epid$t
      }
      incid <- epid$incid
    } 
    
    # 1C- Last resort: can't identify, use 1:length(incid)
    else {
      t <- 1:length(incid)
    }
  } 
  
  # 2- Specific case where incid is of class POSIXt
  else if (inherits(incid, "POSIXt")) {
    stop("incid was given as POSIXt. Please convert to Date first using as.Date.")
  } 
  
  # 3- Presumably, incid is an object from package Epicurve
  else {
    if (length(incid) > 2) {
      
      # Checking if compliant with epicurve format
      epicurve.object <- incid
      if (!is.null(epicurve.object)$dates) {
        # 3A- epicurve.dates
        incid <- table(epicurve.object$dates)
        t <- as.Date(names(table(epicurve.object$dates)))
        
        # 3B- Other epicurve specifications (weeks, month) have a $stratum3 object
        if (!is.null(epicurve.object$stratum3)) {
          incid <- table(epicurve.object$stratum3)
          t <- as.Date(names(table(epicurve.object$stratum3)))
        }
        
        if (is.null(t) | is.null(incid)) {
          stop("Could not find 'dates', 'weeks' or 'month' in epid")
        }
      } 
      
      # If no $dates list is found, not an epicurve object. Stop to prevent unknown behaviour.
      else {
        stop("incid was assumed to be an epicurve object, but doesn't meet epicurve specifications.")
      }
    }
  }
  
  
  # Some more integrity checks: incid is not negative or missing
  if (any(incid < 0) || any(is.na(incid))) {
    stop("'incid' should be positive and non missing")
  }
  
  if (length(incid) != length(t)) {
    stop("'incid' & 't' must have the same length")
  }
  
  return(list(incid = as.vector(incid), 
              t = t))
}
