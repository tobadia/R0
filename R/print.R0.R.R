# Name   : print.R0.R
# Desc   : A tweaked "print" function designed to print useful data on R objects 
#          from any of the supported estimation methods.
# Date   : 2011/11/09
# Update : 2023/03/03
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Print method for objects of class `R0.R`
#' 
#' @description
#' Prints summary statistics for an estimated reproduction ratio.
#' 
#' @details
#' For internal use.
#' 
#' Displays the estimated reproduction ratio along with its confidence interval. 
#' For the TD method, the time-series of \eqn{R(t)} is printed.
#' 
#' @param x An estimated object, output from `est.R0.xx()` routines (class `R0.R`).
#' @param ... Parameters passed to inner functions. 
#' 
#' @return
#' This function does not return any data.
#' Called for side effects.
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

print.R0.R <- function(
    x, 
    ...
) 
  
  # Code
  
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
    
    if (!anyNA(x$conf.int)) {
      cat("[", x$conf.int[1],",",x$conf.int[2],"]\n")
    }
  } 
  
  else {
    #Must be TD/SB... output first 10 values
    if (x$method.code %in% c("TD","SB")) {
      cat(x$R[seq_len(min(length(x$R),10))],"...\n")
    }
  }
  
  cat("\n")
}
