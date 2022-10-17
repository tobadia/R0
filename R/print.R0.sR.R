#' Plot the R0/Rt value along with confidence interval of all requested models
#' to epidemic data
#' 
#' Plot the R0/Rt value along with confidence interval of all requested models
#' to epidemic data
#' 
#' For internal use. Called by print.
#' 
#' Tweaked print() function that prints the reproduction number values for each
#' method contained in the object constructed by est.R0().
#' 
#' @param x Result of est.R0 (class sR))
#' @param \dots Parameters passed to inner functions
#' @return Called for its side effect : Prints all R0 or R(t) values from
#' requested estimation methods.
#' @author Pierre-Yves Boelle, Thomas Obadia
#' @keywords internal
print.R0.sR <- function
(x,
...
)  
{
  #Make sure x is of the right class.
  if (!inherits(x, "R0.sR")) {
    stop("'x' must be of class 'R0.sR'")
  }
  
  #Successive print of individual model
  if (exists("EG", where = x$estimates)) {
    print(x$estimates$EG, ...)
  }
  
  if (exists("ML", where = x$estimates)) {
    print(x$estimates$ML, ...)
  }
  
  if (exists("AR", where = x$estimates)) {
    print(x$estimates$AR, ...)
  }
  
  if (exists("TD", where = x$estimates)) {
    print(x$estimates$TD, ...)
  }
  
  if (exists("SB", where = x$estimates)) {
    print(x$estimates$SB, ...)
  }
}
