\name{fit.epid.optim}
\alias{fit.epid.optim}
\title{Joint estimation of GT distribution and R}
\description{Compute the Poisson log - likelihood between epid and epidemic simulated with R and GT.}
\usage{fit.epid.optim(par = c(1, 1, 1), ...)}
\arguments{
  \item{par}{vector of parameters to be optimised. This should be provided as c(R0, GT.mean, GT.sd)}
  \item{\dots}{parameters passed to inner functions}
}
\details{For internal use. Called by est.R0.ML.
This is a wrapper function used to pass proper arguments to fit.epid when the ML method is used to estimate simultaneously R and GT.
It is used by the \code{optim} routine to find the best-fitting parameters for R and GT (following a Gamma dsitribution)}


\author{Pierre-Yves Boelle, Thomas Obadia}






\keyword{internal}
