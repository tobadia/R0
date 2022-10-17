#' Plot the R0/Rt value along with confidence interval of all requested models
#' to epidemic data
#' 
#' Plot the R0/Rt value along with confidence interval of all requested models
#' to epidemic data
#' 
#' For internal use. Called by plot.
#' 
#' Tweaked plot() function that draws the reproduction number values for each
#' method contained in the object constructed by est.RO().
#' 
#' @param x Result of est.R0 (class sR)
#' @param xscale Scale to be adjusted on X axis. Can be "d" (day), "w" (week
#' (default)), "f" (fornight), "m" (month).
#' @param TD.split Parameter to force the display of both R(t) and the epidemic
#' curve in the same window for TD method
#' @param \dots parameters passed to plot.R
#' @return Called for its side effect : Draws all R0 or R(t) values from
#' requested estimation methods.
#' @author Pierre-Yves Boelle, Thomas Obadia
#' @keywords internal
plot.R0.sR <- function
(x,
 xscale="w",
 TD.split=FALSE,
 ...
) 
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
