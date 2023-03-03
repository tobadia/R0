# Name   : plot.GT
# Desc   : A tweaked "plot" function designed to easily plot GT objects from
#          GT function
# Date   : 2011/11/09
# Update : 2023/03/03
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Plot a generation time distribution
#' 
#' @description
#' Plots the distribution of a generation time object from [generation.time()].
#' 
#' @details
#' For internal use. Called by [base::print()].
#' 
#' @param x Generation time distribution from [generation.time()]. 
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' Called for side effect. Shows a plot of the generation time distribution. 
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

plot.R0.GT <- function(
    x, 
    ... 
) 
  
  
  # Code
  
{
  #Only check is that GT is of class "R0.GT"
  if (!inherits(x, "R0.GT")) {
    stop("GT must be of class 'R0.GT'")
  }
  
  #We plot GT=f(time)
  plot(x$time,x$GT,xlab="Time",ylab="PDF",t='l', main="Generation Time distribution",...)
  
}
