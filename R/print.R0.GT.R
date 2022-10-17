#' Print the characteristics of the generation time distribution
#' 
#' Prints the characteristics of the generation time distribution.
#' 
#' For internal use. Called by print.
#' 
#' @param x the GT distribution.
#' @param \dots Parameters passed to inner functions
#' @return Called for side effect. Displays GT and mean/sd.
#' @author Pierre-Yves Boelle, Thomas Obadia
#' @keywords internal
print.R0.GT <- function
(x,
...
 ) 
{
  #Make sure x is of the right class
	if (!inherits(x, "R0.GT")) {
    stop("x must be of class 'R0.GT'")
  }
	
  cat("Discretized Generation Time distribution\n")
	cat("mean:",x$mean,", sd:",x$sd,"\n")
	print.default(x$GT)
	cat("\n")
}
