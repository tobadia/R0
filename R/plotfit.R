# Name   : plotfit.R
# Desc   : S3 Method for plotting the fit of an epidemic model
# Date   : 2012/05/02
# Update : 2023/03/03
# Author : Boelle, Obadia
###############################################################################


#' @title
#' S3 method for objects of class `R0.R` or `R0.sR`
#' 
#' @description
#' This method ensures compliance with CRAN checks etc. It is used to properly
#' call [plotfit.R0.R()] and [plotfit.R0.sR()] and build corresponding plots.
#' 
#' @details
#' For internal use.
#' 
#' This method is designed to either call [plot.fit.R0.R()] or [plot.fit.R0.sR()], 
#' and complies with S3 requirements at the time of writing.
#' 
#' It allows plotting the goodness-of-fit of an estimated model to the 
#' original epidemic curve provided the user.
#' 
#' Depending on the method of estimation, the graphical output will vary : 
#' * EG, ML and TD methods will show the original epidemic curve, along with the best-fitting prediction model, 
#' * AR will only show the epidemic curve, since no actual model is computed, 
#' * SB will display 9 density curves for the R distribution throughout the epidemic.
#' 
#' @param x An output of [estimate.R()] (class `R0.sR`) or `est.R0.xx()` (class `R0.R`).
#' @param all Boolean. Should the full epidemic curve be shown ?
#' @param xscale Scale to be adjusted on x-axis. Can be `d` (day), `w` (week (default)), `f` (fornight), `m` (month).
#' @param SB.dist Boolean. Should the R distirbution throughout the epidemic be plotted for the SB method (defaults to `TRUE`) ?
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' This function does not return any data.
#' 
#' @export
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

plotfit <- function(
    x, 
    all     = TRUE, 
    xscale  = "w", 
    SB.dist = TRUE, 
    ...
) 
  
  # Code
  
{
  UseMethod("plotfit")
}
