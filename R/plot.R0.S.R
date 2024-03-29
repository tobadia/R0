# Name   : plot.R0.S
# Desc   : A tweaked "plot" function designed to easily plot S objects from
#          sensitivity.analysis.
# Date   : 2011/11/09
# Update : 2023/03/03
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Plot of sensitivity analyses.
#' 
#' @description
#' Generates the graphical output for an object generated through 
#' [sensitivity.analysis()].
#' 
#' @details
#' For internal use. Called by [base::plot()] when applied to `R0.S` objects.
#' A plot will be shown and the best model fit will be returned.
#' 
#' @param x Output of [sensitivity.analysis()] (class `R0.S`)
#' @param what Specify the desired output. Can be `"heatmap"` (default), `"criterion"` or both.
#' @param time.step Optional. If date of first observation is specified, number of day between each incidence observation.
#' @param skip Number of results to ignore (time period in days) when looking for highest Rsquared value.
#' @param ... Parameters passed to inner functions.
#' 
#' @return
#' A list with best R0 measure for each possible time period, along with corresponding begin/end dates.
#' \item{max.Rsquared}{The highest R-squared values.}
#' \item{best.R0.values}{The corresponding \eqn{R_{0}} values.}
#' \item{best.fit}{The best model fit as defined by the highest R-squared values among all returned.}
#' 
#' @importFrom grDevices heat.colors dev.new
#' @importFrom graphics axis filled.contour contour title text legend points
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

plot.R0.S <- function(
    x, 
    what      = "heatmap", 
    time.step = 1, 
    skip      = 5, 
    ... 
)

# Code
  
{
	#Make sure x is of the right class
	if (!inherits(x, "R0.S")) {
    stop("'x' must be of class 'R0.S'")
	}
  
  #Check if 'skip' isn't too high
  if (skip >= length(x$df.clean[,1])) {
    stop("'skip' value is too high, results out of bound.")
  }

  #Extracting duration period from dates (end-begin)
	#fact = as.factor((res$df.clean[,3]-res$df.clean[,2])/time.step)
  fact <- as.factor(x$df.clean[,1])
  
  #Apply this factor to split the results inside data.frame df.clean
  opt.df <- sapply(split(x$df.clean, fact), function(df) {
    rownames(df[which.max(df$Rsquared),])
  })
  
  #opt.df contains the line number in the data.frame where Rsquared is max
  #for each factor level
  max.Rsquared <- x$df.clean[opt.df,]

  # OLD VERSION: Replaced with filled.contour()
  ##And now actual plots are drawned
  #par(xpd=TRUE) allows for legend to be placed outside colored plot
  #if ("heatmap" %in% what) {
  #  par(xpd=TRUE, mar=par()$mar+c(0,0,0,4))
  #  image(x$begin, x$end, t(x$mat.sen), xlab="Begin date (index)", ylab="End date (index)", breaks=c(0, 1, 1.4, 1.5, 1.6, max(x$mat.sen, na.rm=TRUE)), main="Sensitivity of Reproduction ratio with dates", col=rev(heat.colors(5)))
  #  legend(x$begin[1], max(x$end)+0.5,legend=c("<1", "1-1.4", "1.4-1.5", "1.5-1.6", ">1.6"),fill=rev(heat.colors(5)),horiz=T,cex=0.8,bty="n")
  #}
  
  #Other window for best R0 depending on time period
	highest.Rsquared <- max(max.Rsquared$Rsquared[skip:length(max.Rsquared$Rsquared)])
	best.fit <- which(max.Rsquared$Rsquared == highest.Rsquared)
	best.fit <- max.Rsquared[best.fit,]
  
  #Plotting heatmap
	filled.contour(x=x$begin, y=x$end, z=t(x$mat.sen), color.palette=function(t) rev(heat.colors(t)), key.title=(title(main="R0")),                  
    plot.axes={contour(x=x$begin, y=x$end, z=t(x$mat.sen), levels=c(best.fit$CI.lower, best.fit$CI.upper), lwd=2, add=T); 
    axis(1, x$begin);
    axis(2, x$end);
    points(which(x$epid$t == best.fit$Begin.dates), which(x$epid$t == best.fit$End.dates), pch=19);
    text(which(x$epid$t == best.fit$Begin.dates), which(x$epid$t == best.fit$End.dates), paste(round(best.fit$R, 2)), cex=1, pos=4)},
    plot.title=title(main="Sensitivity of Reproduction ratio to begin/end dates", xlab="Begin date (index)", ylab="End date (index)")
  )
  
  if ("criterion" %in% what) {
    #OLD VERSION, NOT PORTABLE
    #x11()
    dev.new()
    
    plot(x=as.numeric(levels(fact))[skip:length(as.numeric(levels(fact)))], y=max.Rsquared$Rsquared[skip:length(max.Rsquared$Rsquared)], type="o", xlab="Time Period", ylab="Maximum Rsquared", main="Goodness of fit (R^2) of the model with time period", ...)
  
    #Highlight highest interesting value
    points(x=best.fit$Time.period, y=best.fit$Rsquared, pch=21, col="red", bg="red")
  }
  
  #Return the max.Rsquared data, as extracted from x$df.clean
  return(list(max.Rsquared=max.Rsquared, best.R0.values=x$df.clean[opt.df,4], best.fit=best.fit))
}
