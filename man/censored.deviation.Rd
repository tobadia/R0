\name{censored.deviation}
\alias{censored.deviation}
\title{Impute censored cases to rebuild longer epidemic vector}
\description{When first records of incidence are unavailable, tries to impute censored cases to rebuild longer epidemic vector}
\usage{censored.deviation(optim.vect, 
    epid, R0, GT)}
\arguments{
  \item{optim.vect}{Vector of two elements (multiplicative factor, log(highest imputed data) correspondig to initial values}
  \item{epid}{Original epidemic vector, output of check.incid()}
  \item{R0}{Assumed \code{R0} value for the original epidemic vector}
  \item{GT}{Generation time distribution to be used for computations}
}
\details{For internal use. Called by \code{\link{impute.incid}}.

This function is not intended for stand-alone use. It computes the difference between theoretical
incidence and observed epidemics, for a given vector of initial values. 
To find the find best-fitting incidence values, this same vector must be optimized, to minimize the
value returned by SCE}
\value{The deviation between sum((E(Nt)-Nt)^2)}

\author{Pierre-Yves Boelle, Thomas Obadia}






\keyword{internal}
