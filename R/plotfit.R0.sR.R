# Name   : plotfit.R0.sR
# Desc   : A tweaked "plot" function designed to easily plot all R objects from
#          a sR-class list (result of estimate.R()).
# Date   : 2011/11/09
# Update : 2023/03/03
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Plot a model fit for `R0.sR` objects
#' 
#' @description
#' Plot the fit of model outputs to epidemic data. 
#' 
#' @details
#' For internal use. This function is called by the [plotfit()] S3 method.
#' It will sequentialy call the necesary [plotfit()] methods for each estimation
#' contained in the `x` argument.
#' 
#' @param x An output of [estimate.R()] (class `R0.sR`).
#' @param xscale Scale to be adjusted on x-axis. Can be `d` (day), `w` (week (default)), `f` (fornight), `m` (month).
#' @param SB.dist Boolean. Should the R distirbution throughout the epidemic be plotted for the SB method (defaults to `TRUE`) ?
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' This function does not return any data.
#' Called for side effect. Draws the fit of one or more estimation method to the 
#' data.
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

plotfit.R0.sR <- function(
    x, 
    xscale  = "w", 
    SB.dist = TRUE, 
    ...
) 
  
  # Code
  
{
  #Make sure x is of the right class.
  if (!inherits(x, "R0.sR")) {
    stop("'x' must be of class 'sR'")
  }
  
  #If invalid scale parameter is used, stop.
  if (xscale != "d" & xscale != "w" & xscale !="f" & xscale != "m") {
    stop("Invalid scale parameter.")
  }
  
  #Successive plots of individual model
  if (exists("EG", where = x$estimates)) {
    plotfit(x$estimates$EG, xscale=xscale, ...)
  }
  
  if (exists("ML", where = x$estimates)) {
    #x11()
    dev.new()
    plotfit(x$estimates$ML, xscale=xscale, ...)
  }
  
  if (exists("TD", where = x$estimates)) {
    #x11()
    dev.new()
    plotfit(x$estimates$TD, xscale=xscale, ...)
  }
  
  if (exists("SB", where = x$estimates)) {
    #x11()
    dev.new()
    plotfit(x$estimates$SB, xscale=xscale, SB.dist = SB.dist, ...)
  }
}
