# Name   : print.GT
# Desc   : A tweaked "print" function designed to print useful data on GT objects 
#          from GT function
# Date   : 2011/11/09
# Update : 2023/03/03
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Print method for objects of class `R0.GT`
#' 
#' @description
#' Prints summary statistics for a generation time distribution from 
#' [generation.time()].
#' 
#' @details
#' For internal use.
#' 
#' Displays the mean and standard deviation for generation time distributions
#' created with [generation.time()], along with the probabilities for each
#' time interval.
#' 
#' @param GT Generation time distribution from [generation.time()]. 
#' @param ... Parameters passed to inner functions. 
#' 
#' @return
#' This function does not return any data.
#' Called for side effects.
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia

# Function declaration

print.R0.GT <- function(
    x, 
    ... 
) 
  
  
  # Code
  
{
  #Make sure x is of the right class
  if (!inherits(x, "R0.GT")) {
    stop("x must be of class 'R0.GT'")
  }
  
  cat("Discretized Generation Time distribution\n")
  cat("mean:",x$mean,", sd:",x$sd,"\n")
  print.default(x$GT)
  cat("\n")
}
