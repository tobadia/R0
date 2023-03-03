# Name   : R.from.r
# Desc   : Calculates the discretized Laplace Transform using a discretized 
#          Generation Time distribution.
# Date   : 2011/11/09
# Update : 2023/03/03
# Author : Boelle, Obadia
###############################################################################


#' @title
#' Estimate reproduction number from exponential growth rate
#' 
#' @description
#' Calculates the basic reproduction number \eqn{R_{0}} from the exponential
#' growth rate obtained by either Poisson or log-linear regression.
#' 
#' @note
#' The formula for the discretized Laplace transform is taken from 
#' Wallinga & Lipsitch (2007).
#'
#' @references
#' Wallinga, J., and M. Lipsitch. "How Generation Intervals Shape the Relationship Between Growth Rates and Reproductive Numbers." Proceedings of the Royal Society B: Biological Sciences 274, no. 1609 (2007): 599.
#'
#' @details
#' For internal use. Called by [est.R0.EG()].
#' 
#' \eqn{R_{0}} is calculated as the inverse of the discretized Laplace transform, 
#' making use of the discretized generation time distirbution (output from 
#' [generation.time()]).
#' 
#' @param r The exponential growth rate.
#' @param GT Generation time distribution from [generation.time()]. 
#' 
#' @return
#' A numeric value for R.
#' 
#' @keywords internal
#' 
#' @author Pierre-Yves Boelle, Thomas Obadia



# Function declaration

R.from.r <- function(
    r, 
    GT
) 
  
  
  # Code
  
{
  Tmax <- length(GT$GT)
  R <- r/sum(GT$GT * (exp(-r*(0:(Tmax-1))) - exp(-r*(1:Tmax))))
}
