#' Print the characteristics of the generation time distribution
#' 
#' Prints the characteristics of the generation time distribution
#' 
#' For internal use. Called by print.
#' 
#' @param x the generation time distribution.
#' @param \dots extra parameters passed to plot.
#' @return Called for side effect. Shows a plot of the generation time
#' distribution.
#' @author Pierre-Yves Boelle, Thomas Obadia
#'
#' @export
plot.R0.GT <- function
(x,
 ...
) 
{
  #Only check is that GT is of class "R0.GT"
	if (!inherits(x, "R0.GT")) {
    stop("GT must be of class 'R0.GT'")
  }
  
  #We plot GT=f(time)
  plot(x$time,x$GT,xlab="Time",ylab="PDF",t='l', main="Generation Time distribution",...)
}
