#' Plot the fit of all requested models to epidemic data
#' 
#' Plots the fit of \code{all} requested models to epidemic data
#' 
#' For internal use. Called by \code{\link{plotfit}}.
#' 
#' Tweaked plot() function that draws the epidemic data and model fit of each
#' method contained in the object constructed by est.RO().
#' 
#' @param x Result of est.R (class R0.R)
#' @param all Should the whole epidemic curve be shown
#' @param xscale Scale to be adjusted on X axis. Can be "d" (day), "w" (week
#' (default)), "f" (fornight), "m" (month).
#' @param SB.dist Should R distribution throughout the epidemic be plotted for
#' SB method? (default: TRUE)
#' @param \dots Parameters passed to plot
#' @return Called for its side effect : Draws \code{all} the epidemic curves
#' and associated fit data computed by est.R0
#' @author Pierre-Yves Boelle, Thomas Obadia
#'
#' @importFrom grDevices dev.new
#'
#' @export
plotfit.R0.sR <- function
(x,
 all=TRUE,
 xscale="w",
 SB.dist=TRUE,
 ...
 ) 
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
