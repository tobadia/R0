# Name   : plot.R0.R
# Desc   : Tweaked plots function to adequately reder estimated R0 or R(t)
#          values, dependng on estimation methods.
# Date   : 2011/11/09
# Update : 2023/03/03
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Plot the R0/Rt value along with confidence interval
#' 
#' @description
#' Generates the graphical output for an estimated \eqn{R} or \eqn{R(t)}. The
#' graph depends on the estimation method. In general, the estimated value is
#' presented along with its confidence interval.
#' 
#' @details
#' For internal use. This function is called when using [plot.R0.sR()].
#' 
#' This is a wrapper function that will call the correct plotting routine 
#' depending on which method was used to estimate a reproduction number.
#' 
#' @param x An estimated object, output from `est.R0.xx()` routines (class `R0.R`).
#' @param xscale Scale to be adjusted on x-axis. Can be `d` (day), `w` (week (default)), `f` (fornight), `m` (month).
#' @param TD.split Boolean. Parameter to force the display of both \eqn{R(t)} and the epidemic curve in the same window for the TD method.
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' This function does not return any data.
#' Called for side effect. Draws all \eqn{R_{0}} or /eqn{R(t)} values from one estimation method.
#' 
#' @importFrom graphics abline arrows axis close.screen split.screen screen legend lines points polygon
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

plot.R0.R <- function(
    x, 
    xscale   = "w", 
    TD.split = FALSE, 
    ... 
) 
  
  # Code
  
{
  #Make sure x is of class "R0.R"
  if (!inherits(x, "R0.R")) stop("'x' must be of class R0.R")
  
  #Adjust arguments depending on method
  if(x$method.code %in% c("EG", "ML", "AR")) {
      do.call(paste0("plotR",x$method.code), args=list(x=x, ...) )
    } else if (x$method.code == "SB") {
      do.call("plotRSB", args=list(x=x, xscale=xscale, ...) )
    } else if (x$method.code == "TD") {
      do.call("plotRTD", args=list(x=x, xscale=xscale, TD.split=TD.split, ...) )
    } else {
      stop("Argument method.code not recognized")
    }
}





#' @title
#' Plot R0 for Exponential Growth method
#' 
#' @description
#' Internal plot method for EG estimates.
#' 
#' @details
#' This plots the exponential growth parameter r, along with R and 
#' corresponding confidence interval.
#' 
#' @param x An estimated object, output from [estimate.R()] with `method = "EG"`.
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' This function does not return any data.
#' Called for side effect.
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Plot for EG
# this plots the exponential growth parameter r, along with R
plotREG <- function(
    x, 
    ...
)
  
{
  R <- x$R
  conf.int <- x$conf.int
  r <- x$r
  
  r.seq = seq(from=-1, to=1, by=0.01)
  exp.r.seq = exp(r.seq*x$GT$mean)
  constant.r.seq = 1+r.seq*x$GT$mean
  
  plot(y=exp.r.seq, x=r.seq, 
       xlim=c(-1,1), ylim=c(0,3), 
       type="l", lty="dashed", 
       xlab="r (Growth rate)", ylab="R (Reproduction number)", 
       main=paste("Reproduction number (", x$method,")"), ...)
  lines(y=constant.r.seq, x=r.seq, xlim=c(-1,1), ylim=c(0,3), lty="solid")
  
  points(y=R, x=r, col="blue")
  arrows(r,conf.int[2], r, conf.int[1], angle=90, code=3, col="blue", length=0.03)
  legend(x="bottomright", c("exp(r*mean(GT))", "1+r*mean(GT)", "R"), pch=c(NA_integer_, NA_integer_, 21), 
         col=c("black", "black", "blue"), lty=c("dashed", "solid", "blank"), merge=TRUE)
}





#' @title
#' Plot R0 for Attack Rate method
#' 
#' @description
#' Internal plot method for AR estimates.
#' 
#' @details
#' This plots the R0 reproduction number along with confidence interval.
#' 
#' @param x An estimated object, output from [estimate.R()] with `method = "AR"`.
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' This function does not return any data.
#' Called for side effect.
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Plot for AR
# this plots the estimated R along with confidence interval
plotRAR <- function(
    x, 
    ...
)
  
{
  R <- x$R
  conf.int <- x$conf.int
  
  plot(x=1, y=R, col="blue", 
       ylab="R0 value", xaxt="n", 
       xlab="", 
       main=paste("Reproduction number (", x$method,")"), ...)
  arrows(1,conf.int[2], 1, conf.int[1], angle=90, code=3, col="blue", length=0.03)   
}





#' @title
#' Plot R0 for Maximum Likelihood method
#' 
#' @description
#' Internal plot method for ML estimates.
#' 
#' @details
#' This plots the R0 reproduction number along with confidence interval.
#' 
#' @param x An estimated object, output from [estimate.R()] with `method = "ML"`.
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' This function does not return any data.
#' Called for side effect.
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Plot for ML
# this plots the estimated R along with confidence interval
plotRML <- function(
    x, 
    ...
)
  
{
  
  R <- x$R
  conf.int <- x$conf.int
  
  plot(x=1, y=R, col="blue", 
       ylab="R (Reproduction number)", xaxt="n", xlab="", 
       main=paste("Reproduction number (", x$method,")"), ...)
  arrows(1,conf.int[2], 1, conf.int[1], angle=90, code=3, col="blue", length=0.03)   
}





#' @title
#' Plot R0 for Time-Dependent method
#' 
#' @description
#' Internal plot method for TD estimates.
#' 
#' @details
#' This plots the time-dependent \eqn{R(t)} reproduction number along with 
#' confidence band.
#' 
#' @param x An estimated object, output from [estimate.R()] with `method = "TD"`.
#' @param xscale Scale to be adjusted on x-axis. Can be `d` (day), `w` (week (default)), `f` (fornight), `m` (month).
#' @param TD.split Boolean. Parameter to force the display of both \eqn{R(t)} and the epidemic curve in the same window for the TD method.
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' This function does not return any data.
#' Called for side effect.
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Plot for TD
# this plots the estimated R(t) along with confidence band
plotRTD <- function(
    x, 
    xscale, 
    TD.split,
    ...
)
  
{
  #We plot R(t) values along with 95CI
  R <- x$R
  epid <- x$epid
  epid$t <- epid$t[x$begin.nb:(x$end.nb-1)]
  polygon.x <- c(epid$t, rev(epid$t))
  polygon.y <- c(x$conf.int[seq_along(epid$t),1], rev(x$conf.int[seq_along(epid$t),2]))
  
  div <- get.scale(xscale)
  #Where should labels be on axis
  atLab <- pretty(epid$t, n=length(epid$t)/div)
  #What should labels say
  lab <- format(pretty(epid$t, n=length(epid$t)/div))
  
  if (TD.split) {
    #par(bg="white")
    split.screen(c(2,1))
    screen(1)
    plot(epid$incid[x$begin.nb:x$end.nb], 
         type='s', xlab="Time", ylab="Incidence", 
         xaxt="n", main="")
    
    axis(1, at=atLab, labels=lab)
    screen(2)
  }
  plot(epid$t, R[seq_along(epid$t)], ylim=c(0, max(polygon.y)), xlab="Time", ylab="R(t)", xaxt="n", pch=NA_integer_, lty="blank", main=paste("Reproduction number (", x$method,")"), ...)
  polygon(polygon.x, polygon.y, col="gray", border=NA)
  lines(epid$t, R[seq_along(epid$t)])
  abline(h=1, lty="dashed", col="gray40")
  
  #Blue = lowest quantile (default: 5%)
  #Red = highest quantile (default: 95%)
  #points(epid$t, Rt.quant$CI.lower.[seq_along(epid$t)], col="blue", xaxt="n", pch=NA_integer_)
  #points(epid$t, Rt.quant$CI.upper.[seq_along(epid$t)], col="red", xaxt="n", pch=NA_integer_)
  axis(1, at=atLab, labels=lab)
  
  if (TD.split) {
    close.screen(split.screen())
  }
}





#' @title
#' Plot R0 for Sequential Bayesian method
#' 
#' @description
#' Internal plot method for SB estimates.
#' 
#' @details
#' This plots the bayesian estimation of \eqn{R_{0}} along with its 
#' credible interval.
#' 
#' @param x An estimated object, output from [estimate.R()] with `method = "SB"`.
#' @param xscale Scale to be adjusted on x-axis. Can be `d` (day), `w` (week (default)), `f` (fornight), `m` (month).
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' This function does not return any data.
#' Called for side effect.
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Plot for SB
# this plots the estimated R distribution along with credible interval
plotRSB <- function(
    x, 
    xscale, 
    ...
)
  
{
  #We plot R(t) values along with 95CI
  R= x$R
  epid = x$epid
  
  epid$t = epid$t[x$begin.nb:(x$end.nb-1)]
  polygon.x = c(epid$t, rev(epid$t))
  polygon.y = c(x$conf.int[seq_along(epid$t),1], rev(x$conf.int[seq_along(epid$t),2]))
  
  #plot(epid$t, Rt.quant$R.t.[seq_along(epid$t)], ylim=c(0, max(polygon.y)), xlab="Time", ylab="R(t)", xaxt="n", main=paste("Reproduction number (", x$method,")"),...)
  plot(epid$t, R[seq_along(epid$t)], ylim=c(0, max(polygon.y)), xlab="Time", ylab="R(t)", xaxt="n", pch=NA_integer_, lty="blank", 
       main=paste("Reproduction number (", x$method,")"), ...)
  polygon(polygon.x, polygon.y, col="gray", border=NA)
  lines(epid$t, R[seq_along(epid$t)])
  abline(h=1, lty="dashed", col="gray40")
  
  div = get.scale(xscale)
  #Where should labels be on axis
  atLab = pretty(epid$t, n=length(epid$t)/div)
  #What should labels say
  lab = format(pretty(epid$t, n=length(epid$t)/div))
  
  axis(1, at=atLab, labels=lab)
}





#' @title
#' Scaling of x-axis
#' 
#' @description
#' Internal scaling function to display proper x-axis labels.
#' 
#' @details
#' Builds the x-axis labels corresponding to a human-friendly level (day, week...).
#' 
#' @param scale Scale to be adjusted on x-axis. Can be `d` (day), `w` (week (default)), `f` (fornight), `m` (month).
#' 
#' @return
#' An integer corresponding to the number of days between each x-axis tickmark.
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



get.scale <- function(
    scale
)
  
{
  #Scale parameters are used to adjust dates on X-Axis labels
  if (scale == "d") {
    div <- 1
  } else if (scale == "w") {
    div <- 7
  } else if (scale == "f") {
    div <- 15
  } else if (scale == "m") {
    div <- 30
  } else {
    stop("Invalid scale parameter.")
  }
  return(div)
}
