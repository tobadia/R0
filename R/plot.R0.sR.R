# Name   : plot.R0.sR
# Desc   : A tweaked "plot" function designed to easily plot all R objects from
#          a R0.sR-class list (result of estimate.R).
# Date   : 2011/11/09
# Update : 2023/03/03
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Plot the R0/Rt value along with confidence interval
#' 
#' @description
#' Generates the graphical output for estimated \eqn{R} or \eqn{R(t)} fits.
#' 
#' 
#' @details
#' For internal use. Called by [base::plot())].
#' 
#' Tweaked [base::plot()] function that draws the reproduction number values
#' for each method contained in the object constructed by [estimate.R()].
#' 
#' @param x An output of [estimate.R()] (class `R0.sR`)
#' @param xscale Scale to be adjusted on x-axis. Can be `d` (day), `w` (week (default)), `f` (fornight), `m` (month).
#' @param TD.split Boolean. Parameter to force the display of both \eqn{R(t)} and the epidemic curve in the same window for the TD method.
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' This function does not return any data.
#' Called for side effect. Draws all \eqn{R_{0}} or /eqn{R(t)} values from an 
#' output of [estimate.R()], built with one or more methods.
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

plot.R0.sR <- function(
    x, 
    xscale   = "w", 
    TD.split = FALSE, 
    ... 
) 
  
  
  # Code
  
{
  #Make sure x is of the right class.
  if (!inherits(x, "R0.sR")) {
    stop("'x' must be of class 'R0.sR'")
  }
  
  #If invalid scale parameter is used, stop.
  if (xscale != "d" & xscale != "w" & xscale !="f" & xscale != "m") {
    stop("Invalid scale parameter.")
  }
  
  #Successive plots of individual model
  if (exists("EG", where = x$estimates)) {
    plot(x$estimates$EG, xscale=xscale, ...)
  }
  
  if (exists("ML", where = x$estimates)) {
    #x11()
    dev.new()
    plot(x$estimates$ML, xscale=xscale, ...)
  }
  
  if (exists("AR", where = x$estimates)) {
    #x11()
    dev.new()
    plot(x$estimates$AR, xscale=xscale, ...)
  }
  
  if (exists("TD", where = x$estimates)) {
    #x11()
    dev.new()
    plot(x$estimates$TD, xscale=xscale, TD.split=TD.split, ...)
  }
  
  if (exists("SB", where = x$estimates)) {
    #x11()
    dev.new()
    plot(x$estimates$SB, xscale=xscale, ...)
  }
  
}
