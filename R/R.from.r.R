#' Compute the discretized Laplace Transform using a discretized Generation
#' Time distribution
#' 
#' Computes the discretized Laplace Transform using a discretized Generation
#' Time distribution.
#' 
#' For internal use. Called by \code{\link{est.R0.EG}}.
#' 
#' @param r exponential growth ratio
#' @param GT discretized generation time distribution
#' @return An R value corresponding to inverse of discretized Laplace
#' transform.
#' @note The formula for the discretized Laplace transform is taken from
#' Wallinga.
#' @author Pierre-Yves Boelle, Thomas Obadia
#' @keywords internal
R.from.r = function
(r,
GT
) 
{
	Tmax = length(GT$GT)
	R = r/sum(GT$GT * (exp(-r*(0:(Tmax-1))) - exp(-r*(1:Tmax))))
}
