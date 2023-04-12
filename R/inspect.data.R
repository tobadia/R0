# Name   : inspect.data
# Desc   : Convert primary input as an array with incidence and related date
# Date   : 2020/07/28
# Update : 2023/03/03
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Audit input data for common issues
#' 
#' @description
#' Inspect input data and look for common mistakes. 
#' The function does not return any object but yields warnings wupon detecting 
#' possible inconsistencies, along with suggestions as to how to clean inputs 
#' before running estimation routines.
#' 
#' @details
#' [inspect.data()] looks for common issues that could affect estimation routines. 
#' Such issues include too low incidence counts, leading/trailing zeros, 
#' non-integer values...
#' 
#' Before any checks are conducted, the data are passed to [check.incid()] to 
#' try and guess the format of the data.
#' 
#' A not-so-uncommon issue is to provide non-integer counts for incidence, for 
#' example when working with aggregated data that represent averaged number of 
#' cases across different communities. This however does not agree well with 
#' parametric likelihood that assume exponential growth over the early stage of 
#' an epidemic or Poisson distribution of cases, where non-integer values will 
#' cause calculations to fail.
#'
#' Missing values may cause issues if not handled properly. By default, 
#' [check.incid()] will recast missing values to zero. Leading and trailing `NA`'s 
#' should be omitted entirely from the input. Gaps found between available data 
#' may also cause issues if they span over a period that's longer than the total 
#' generation time. A warning is raised to inform on these possible issues.
#' 
#' Likewise, leading and tailing zeros would cause similar issues. Begin will 
#' default to the first value and end to the peak one. Just in case, these will 
#' be inspected here too. Sequence of 0s exceeding the length of the generation 
#' time will also yield a warning.
#' 
#' Scarce data may also cause errors when optimizing likelihood functions. 
#' A time-series of incidence spanning for a duration shorter than that of the 
#' generation time distribution is likely to correspond to an index case that hasn't 
#' yet infected all its offsprings. This would biais estimates downwards and should 
#' be taken into account when interpreting results.
#' 
#' @param incid An object (vector, data.frame, list) storing incidence.
#' @param GT Generation time distribution from [generation.time()]. 
#' @param t Vector of dates at which incidence was observed (optional). 
#' 
#' @return
#' No object is returned. Instead, warnings are thrown upon detecting inconsistences.
#' 
#' @export
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

inspect.data <- function(
    incid, 
    GT = NULL, 
    t  = NULL 
)
  
  # Code
  
{
  
  # Start by guessing the input format with check.incid
  epid <- try(check.incid(incid = incid, 
                          t = t))
  
  if (inherits(epid, "try-error")) warning("check.incid() failed to process inputs and should have provided an explanation.")
  
  # The interesting bits: when check.incid() succeeds, look for non-obvious issues
  else {
    if (any(round(epid$incid) != epid$incid)) {
      warning("Data does not consist only of integer values. Consider rounding to avoid computational issues.")
    }
    
    if (any(is.na(incid))) {
      warning("Warning: your data contains missing values. These will be automatically converted to zeros by check.incid() and may affect your estimates.")
      
      # Leading NAs are detected if 1st value is NA
      if (is.na(incid[1])) {
        idx.last.leading.missing <- diff(cumsum(is.na(incid)))
        idx.last.leading.missing <- 1 + sum(idx.last.leading.missing[1:(min(which(idx.last.leading.missing == 0)) - 1)])
        warning("There are trailing missing values in your data. Consider subsetting to remove data points 1 through ", idx.last.leading.missing, ".")
      }
      
      # Trailing NAs are detected the same way on the reversed incid vector
      if (is.na(incid[length(incid)])) {
        idx.last.trailing.missing <- diff(cumsum(is.na(rev(incid))))
        idx.last.trailing.missing <- sum(idx.last.trailing.missing[1:(min(which(idx.last.trailing.missing == 0)) - 1)])
        warning("There are trailing missing values in your data. Consider subsetting to remove data points ", length(incid) - idx.last.trailing.missing, " through ", length(incid), ".")
      }
      
      # If GT is provided, check that NAs do not exceed its length, which would result in failed optimization routines.
      if (!is.null(GT)) {
        # Confirm first that GT is of the proper class, fail otherwise
        if (!inherits(GT, "R0.GT")) {
          stop("GT was not provided as an object of class R0.GT. See ?generation.time for more detail.")
        } else {
          longest.missing.length <- unlist(lapply(split(!is.na(incid)[is.na(incid)], cumsum(!is.na(incid))[is.na(incid)]), 
                                                  length))
          if (max(longest.missing.length, na.rm = TRUE) >= length(GT$GT)) {
            warning("Data contains a sequence of missing values that is at least as long as the generation time distribution. This will cause issues with optimization routines.")
          }
        }
      }
    }
    
    if (any(epid$incid == 0)) {
      # Leading zeros may cause an issue
      if (epid$incid[1] == 0) {
        idx.last.leading.zero <- diff(cumsum(epid$incid == 0))
        idx.last.leading.zero <- 1 + sum(idx.last.leading.zero[1:(min(which(idx.last.leading.zero == 0)) - 1)])
        warning("There are leading zeros in your data. Consider subsetting to remove data points 1 through ", idx.last.leading.zero, ".")
      }
      
      # Trailing zeros should not be too much of an issue, but are still checked
      if (epid$incid[length(incid)] == 0) {
        idx.last.trailing.zero <- diff(cumsum(rev(epid$ncid) == 0))
        idx.last.trailing.zero <- sum(idx.last.trailing.zero[1:(min(which(idx.last.trailing.zero == 0)) - 1)])
        warning("There are trailing zeros in your data. Consider subsetting to remove data points ", length(epid$incid) - idx.last.trailing.zero, " through ", length(epid$incid), ".")
      }
      
      # If GT is provided, check that consecutive 0s do not exceed its length
      if (!is.null(GT)) {
        # Confirm first that GT is of the proper class, fail otherwise
        if (!inherits(GT, "R0.GT")) {
          stop("GT was not provided as an object of class R0.GT. See ?generation.time for more detail.")
        } else {
          longest.zero.length <- unlist(lapply(split(epid$incid[epid$incid == 0], cumsum(epid$incid != 0)[epid$incid == 0]), 
                                               length))
          if (max(longest.zero.length, na.rm = TRUE) >= length(GT$GT)) {
            warning("Data contains a sequence of zeros that is at least as long as the generation time distribution. This will cause issues with optimization routines.")
          }
        }
      }
    }
    
    if (!is.null(GT)) {
      # Confirm first that GT is of the proper class, fail otherwise
      if (!inherits(GT, "R0.GT")) {
        stop("GT was not provided as an object of class R0.GT. See ?generation.time for more detail.")
      } else {
        # Check vector length since first non-0 value and compare to length of GT
        if (length(epid$incid[min(which(epid$incid > 0), na.rm = TRUE):length(epid$incid)]) < length(GT$GT)) {
          warning("Length of incidence count is shorter than that of generation time. Estimates will likely be biased downwards.")
        }
      }
    }
  }
}
