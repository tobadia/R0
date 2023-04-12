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
  #Various class and integrity checks
  if (is.numeric(incid) || is.character(incid) || inherits(incid, "Date")) {
    
    #Basic possibility is incidence provided by date of symptoms onset for each patient
    if (is.character(incid) || inherits(incid, "Date")) {
      # try to convert incid to dates using standard formats "%Y-%m-%d" / "%Y/%m/%d"
      tmp <- as.Date(incid)
      # if passed, incid was a vector of dates - convert to factor, making sure that levels correspond to min - max by time-step
      # check that no dates are closer together than time.step  
      if ( as.numeric(min(diff(sort(unique(tmp))))) < time.step) {
        idx <- which(as.numeric(diff(sort(unique(tmp)))) < time.step)[1]
        dt.too.close <- sort(unique(tmp))[(idx-1):idx]      
        stop("dates ", paste(dt.too.close,collapse=";")," must be at least time.step = ",time.step," units apart from each other")
      }
      t <- as.character(seq(min(tmp,na.rm=T), max(tmp,na.rm=T),by=time.step))      
      incid <- as.numeric(table(factor(incid, levels=t)))
    }
    
    #No dates provided, use names(t) if available, or integers from 1 to length(incid) if not
    if (is.null(t)) {
      if (is.null(date.first.obs)) {
        
        if (!is.null(names(incid))) {
          t <- names(incid)
          
        }
        else {
          t <- 1:length(incid)
          t[-1] <- t[-1]*time.step - (time.step-1)
        }
      }
      else {
        t <- seq(from=as.Date(date.first.obs), to=time.step*length(incid)+as.Date(date.first.obs)-1, by=time.step)
      }
    }
    
    #Try to determine if t is numeric or date
    if (!any(is.na(suppressWarnings(as.numeric(t)))) && !inherits(t, "Date")) {
      #names are numeric 
      t <- as.numeric(t)
      incid <- incid[order(t)]
      t <- t[order(t)]
      if (length(unique(t)) != length(t)) {stop("duplicates t values or duplicate names in incid")}
      # check that all t values are present with at last time.step interval
      if (min(diff(t)) < time.step) stop("t values must be at least time.step = ",time.step," units apart from each other")
      tmp <- merge(data.frame(t=t, incid=incid),data.frame(t=seq(min(t,na.rm=T),max(t,na.rm=T),by=time.step)),all.y=T)
      tmp$incid[is.na(tmp$incid)] <- 0
      incid <- tmp$incid
      t <- tmp$t
    } 
    
    #Try dates with most common format
    else if (suppressWarnings((!is.na(strptime(t[1], format="%Y-%m-%d"))) | (!is.na(strptime(t[1], format="%Y/%m/%d"))))) {
      t <- as.Date(t)
      incid <- incid[order(t)]
      t <- t[order(t)]
      if (length(unique(t)) != length(t)) {stop("duplicates t values or duplicate names in incid")}
      # check that all t values are present with at last time.step interval
      if (min(as.numeric(diff(t))) < time.step) stop("t values must be at least time.step = ",time.step," units apart from each other")
      tmp <- merge(data.frame(t=t, incid=incid),data.frame(t=seq(min(t,na.rm=T),max(t,na.rm=T),by=time.step)),all.y=T)
      tmp$incid[is.na(tmp$incid)] <- 0
      incid <- tmp$incid
      t <- as.Date(as.character(tmp$t))
    }
    
    #Doesn't match any date format or custom numeric sequence
    else {
      #replace t by integer sequence
      t <- 1:length(incid)
    }
  } 
  
  #t is provided along with incidence : 
  
  else if (inherits(incid, "POSIXt")) {
    stop("incid was given as POSIXt object. Please convert to Date first using as.Date.")
  } else {
    
    #Incid is from any epicurve methods
    if (length(incid) > 2) {
      
      #epicurve object is assumed. Checking if compliant with epicurve format
      epicurve.object <- incid
      if (!is.null(epicurve.object)$dates) {
        #Basic case: epicurve.dates
        incid <- table(epicurve.object$dates)
        t <- as.Date(names(table(epicurve.object$dates)))
        
        #If object was from more precise epicurve method (weeks, month), there must be a $stratum3 object
        if (!is.null(epicurve.object$stratum3)) {
          incid <- table(epicurve.object$stratum3)
          t <- as.Date(names(table(epicurve.object$stratum3)))
        }
        
        if (is.null(t) || is.null(incid)) {
          stop("Could not find 'dates', 'weeks' or 'month' in epid")
        }
      } 
      
      #If no $dates list is found, not an epicurve object. Stop to prevent unknown behaviour.
      else {
        stop("incid was assumed to be an epicurve object, but doesn't meet epicurve specifications.")
      }
      
    }
    
    
  }
  
  #Incid is not negative or missing
  if (any(incid < 0) || any(is.na(incid))) {
    stop("'incid' should be positive and non missing")
  }
  
  if (length(incid) != length(t)) {
    stop("'incid' & 't' must have the same length")
  }
  
  return(list(incid=as.vector(incid),t=t))
}
