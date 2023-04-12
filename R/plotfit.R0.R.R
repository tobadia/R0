# Name   : plotfit.R0.R
# Desc   : A set of tweaked "plot" functions designed to easily plot R objects 
#          from any of the supported estimation methods.
# Date   : 2011/11/09
# Update : 2023/03/03
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Plot a model fit for `R0.R` objects
#' 
#' @description
#' Plot the fit of a single model output to epidemic data. 
#' 
#' @details
#' For internal use. This function is called by the [plotfit()] S3 method.
#' Depending on the estimation method, either [plotfitRxx()], [plotfitRAR()] or
#' [plotfitRSB()] will be called.
#' 
#' @param x An output of `est.R0.xx()` (class `R0.R`).
#' @param xscale Scale to be adjusted on x-axis. Can be `d` (day), `w` (week (default)), `f` (fornight), `m` (month).
#' @param SB.dist Boolean. Should the R distirbution throughout the epidemic be plotted for the SB method (defaults to `TRUE`) ?
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' This function does not return any data.
#' Called for side effect. Draws the fit of one estimation method to the data.
#' 
#' @importFrom grDevices dev.new
#' @importFrom graphics abline axis close.screen split.screen screen lines points
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

plotfit.R0.R <- function(
    x, 
    xscale  = "w", 
    SB.dist = TRUE, 
    ... 
) 
  
  # Code
  
{
  #Make sure x is of class "R0.R"
  if (!inherits(x, "R0.R")) stop("'x' must be of class R0.R")
  
  if (x$method.code %in% c("EG","ML","TD")) {
    do.call(plotfitRxx, args=list(x=x, xscale=xscale, ...) )
  }
  else {
    do.call(paste("plotfitR",x$method.code,sep=""), args=list(x=x, xscale=xscale, SB.dist=SB.dist, ...) )
  }
  
}





#' @title
#' Internal plotfit method for EG, ML and TD estimates
#' 
#' @description
#' Plot the fit of a single model output to epidemic data when the method is
#' EG, ML or TD.
#' 
#' @details
#' For internal use. This function is called by the [plotfit.R0.R()].
#' 
#' @param x An output of [est.R0.EG()], [est.R0.ML()] or [est.R0.TD()] (class `R0.R`).
#' @param xscale Scale to be adjusted on x-axis. Can be `d` (day), `w` (week (default)), `f` (fornight), `m` (month).
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' This function does not return any data.
#' Called for side effect. Draws the fit of one estimation method to the data.
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Generic EG, ML and TD plotfit
plotfitRxx <- function(
    x, 
    xscale, 
    ...
)
  
  # Code
  
{
  epid <- x$epid
  
  #Get data used for the fit
  begin <- x$begin
  begin.nb <- x$begin.nb
  end <- x$end
  end.nb <- x$end.nb
  
  epid.used.for.fit <- list(incid=epid$incid[begin.nb:end.nb], t=epid$t[begin.nb:end.nb])
  
  #Plot the whole original epidemic data
  plot(epid$t,epid$incid, xlab="Time",ylab="Incidence",t='s', xaxt="n", main=paste("Epidemic curve & model (", x$method,")"), ...)
  
  #Add a line showing predicted simulation
  lines(epid.used.for.fit$t,x$pred,col='red')
  
  #Highlight the original points
  points(epid.used.for.fit$t,epid.used.for.fit$incid,pch=19)
  
  #Finally, X-Axis labels
  div <- get.scale(xscale)
  #Where should labels be on axis
  atLab <- pretty(epid$t, n=length(epid$t)/div)
  #What should labels say
  lab <- format(pretty(epid$t, n=length(epid$t)/div))
  axis(1, at=atLab, labels=lab)
  
}





#' @title
#' Internal plotfit method for AR estimates
#' 
#' @description
#' Plot the fit of a single model output to epidemic data when the method is AR.
#' 
#' @details
#' For internal use. This function is called by the [plotfit.R0.R()].
#' 
#' @param x An output of [est.R0.AR()] (class `R0.R`).
#' @param xscale Scale to be adjusted on x-axis. Can be `d` (day), `w` (week (default)), `f` (fornight), `m` (month).
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' This function does not return any data.
#' Called for side effect. Draws the fit of one estimation method to the data.
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# AR plotfit
plotfitRAR <- function(
    x, 
    xscale,
    ...
)
  
  # Code
  
{
  epid <- x$epid
  epid.orig <- x$epid.orig
  epid.used.for.fit <- list(incid=epid.orig$incid, t=epid.orig$t)
  
  #Plot the whole original epidemic data
  plot(epid$t,epid$incid, xlab="Time",ylab="Incidence",t='s', xaxt="n", main="Epidemic curve (Attack Rate)", ...)
  
  #Highlight the original points
  points(epid.used.for.fit$t,epid.used.for.fit$incid,pch=19)
  
  #Finally, X-Axis labels
  div <- get.scale(xscale)
  #Where should labels be on axis
  atLab <- pretty(epid$t, n=length(epid$t)/div)
  #What should labels say
  lab <- format(pretty(epid$t, n=length(epid$t)/div))
  axis(1, at=atLab, labels=lab)
}





#' @title
#' Internal plotfit method for AR estimates
#' 
#' @description
#' Plot the fit of a single model output to epidemic data when the method is SB.
#' 
#' @details
#' For internal use. This function is called by the [plotfit.R0.R()].
#' 
#' @param x An output of [est.R0.SB()] (class `R0.R`).
#' @param xscale Scale to be adjusted on x-axis. Can be `d` (day), `w` (week (default)), `f` (fornight), `m` (month).
#' @param SB.dist Boolean. Should the R distirbution throughout the epidemic be plotted for the SB method (defaults to `TRUE`) ?
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' This function does not return any data.
#' Called for side effect. Draws the fit of one estimation method to the data.
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# SB plotfit
plotfitRSB <- function(
    x, 
    xscale, 
    SB.dist, 
    ...
)
  
  # Code
  
{
  epid <- x$epid
  
  #Get data used for the fit
  begin <- x$begin
  begin.nb <- x$begin.nb
  end <- x$end
  end.nb <- x$end.nb
  
  epid.used.for.fit <- list(incid=epid$incid[begin.nb:end.nb], t=epid$t[begin.nb:end.nb])
  
  #Plot the whole original epidemic data
  plot(epid$t,epid$incid, xlab="Time",ylab="Incidence",t='s', xaxt="n", main=paste("Epidemic curve & model (", x$method,")"), ...)
  
  #Add a line showing predicted simulation
  lines(epid.used.for.fit$t,x$pred,col='red')
  
  #Highlight the original points
  points(epid.used.for.fit$t,epid.used.for.fit$incid,pch=19)
  
  #Finally, X-Axis labels
  div <- get.scale(xscale)
  #Where should labels be on axis
  atLab <- pretty(epid$t, n=length(epid$t)/div)
  #What should labels say
  lab <- format(pretty(epid$t, n=length(epid$t)/div))
  axis(1, at=atLab, labels=lab)
  
  #When plotting Bayesian, if SB.dist is enabled, plot some R distributions throughout the epidemic
  if (SB.dist) {
    #x11()
    dev.new()
    split.screen(c(3,3))
    if (end.nb-begin.nb>8) {
      num.to.plot <- c(1, rep(NA, 8))
    }
    else {
      num.to.plot <- c(begin.nb:end.nb)
    }
    for (i in seq_along(num.to.plot)) {
      if (i == 1) {
        screen(1)
        plot(y=x$proba.Rt[[num.to.plot[i]]], x=seq(from=0, to=(length(x$proba.Rt[[num.to.plot[i]]])/100-0.01), by=0.01), xlab="R value", ylab="PDF", type="l", main=paste("t=",num.to.plot[i]), ...)
        abline(v=(which.max((cumsum(x$proba.Rt[[num.to.plot[i]]])) >= 0.025)-1)/100, col="red", lty="dotted")
        abline(v=(which.max((cumsum(x$proba.Rt[[num.to.plot[i]]])) >= 0.975)-1)/100, col="red", lty="dotted")
        next
      }
      if (is.na(num.to.plot[i])) {
        num.to.plot[i] <- num.to.plot[i-1] + floor(length(x$epid$incid[begin.nb:end.nb])/9)
      }
      
      screen(i)
      plot(x$proba.Rt[[num.to.plot[i]]], x=seq(from=0, to=(length(x$proba.Rt[[num.to.plot[i]]])/100-0.01), by=0.01), xlim=c(0,((length(x$proba.Rt[[num.to.plot[i]]]) - which.max(rev(x$proba.Rt[[num.to.plot[i]]])>0) + 1))/100 - 0.01), xlab="R value", ylab="PDF", pch=NA_integer_, type="l", main=paste("t=",num.to.plot[i]), ...)
      abline(v=(which.max((cumsum(x$proba.Rt[[num.to.plot[i]]])) >= 0.025)-1)/100, col="red", lty="dotted")
      abline(v=(which.max((cumsum(x$proba.Rt[[num.to.plot[i]]])) >= 0.975)-1)/100, col="red", lty="dotted")
      
    }
    #Closing devices
    close.screen(all.screens=TRUE)
  }
}
