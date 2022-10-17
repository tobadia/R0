# Name   : print.R
# Desc   : A tweaked "print" function designed to print useful data on R objects 
#          from any of the supported estimation methods.
# Date   : 2011/11/09
# Author : Boelle, Obadia
###############################################################################


# Function declaration



#' Print the R estimate
#' 
#' Prints the R estimate
#' 
#' For internal use. Called by print.
#' 
#' @param x Result of est.R0.xx (class R0.R)
#' @param \dots Parameters passed to inner functions
#' @return Called for side effect.
#' @author Pierre-Yves Boelle, Thomas Obadia
#' @keywords internal
print.R0.R <- function
(x,
 ... 
 ) 
{
	#Make sure x is of the right class
	if (!inherits(x, "R0.R")) {
    stop("'x' must be of class 'R0.R'")
	}

	cat("Reproduction number estimate using ",x$method," method.\n")
  
  #Next step should be to print the call used to compute this value... but buggy for now
  #print(paste("> ", x$call,"\n", sep=""))
	
  #If not TD/SB, then output R and its 95% CI
	if (x$method.code %in% c("EG","ML","AR")) {
		cat("R : ",x$R)
		
    if (!any(is.na(x$conf.int))) {
		  cat("[", x$conf.int[1],",",x$conf.int[2],"]\n")
		}
	} 
  
  else {
		#Must be TD/SB... output first 10 values
    if (x$method.code %in% c("TD","SB")) {
		  cat(x$R[1:min(length(x$R),10)],"...\n")
    }
	}
	
  cat("\n")
}
