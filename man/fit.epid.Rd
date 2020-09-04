\name{fit.epid}
\alias{fit.epid}
\title{Compute the Poisson log - likelihood between epid and epidemic simulated with R and GT}
\description{Compute the Poisson log - likelihood between \code{epid} and epidemic simulated with R and \code{GT}.}
\usage{fit.epid(log.R, epid, 
    GT, import, pred = FALSE, 
    offset = 0)}
\arguments{
  \item{log.R}{log Reproduction ratio}
  \item{epid}{epidemic}
  \item{GT}{Generation time distribution.}
  \item{import}{Vector of imported cases.}
  \item{pred}{Returns either the predictive curve or the log-likelihood value (default)}
  \item{offset}{to \code{offset} (confidence interval)}
}
\details{For internal use. Called by \code{\link{est.R0.ML}}.

For internal use. Called from est.ML.R0.	
Compute the Poisson likelihood of epidemic.}
\value{Returns a Poisson log-likelihood with given R and \code{GT}}

\author{Pierre-Yves Boelle, Thomas Obadia}






\keyword{internal}
