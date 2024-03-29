% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute.incid.R
\name{impute.incid}
\alias{impute.incid}
\title{Optimiziation routine for incidence imputation}
\usage{
impute.incid(CD.optim.vect, CD.epid, CD.R0, CD.GT)
}
\arguments{
\item{CD.optim.vect}{Vector of two elements (\verb{c(multiplicative factor, log(highest imputed data))}) to be optimized.}

\item{CD.epid}{Original epidemic vector, output of \code{\link[=check.incid]{check.incid()}}.}

\item{CD.R0}{Assumed R0 value for the original epidemic vector.}

\item{CD.GT}{Generation time distribution to be used for computations.}
}
\value{
A vector with both imputed incidence and source available data.
}
\description{
When first records of incidence are unavailable, tries to impute censored
cases to rebuild a longer epidemic vector.
}
\details{
This function is not intended for stand-alone use. It optimizes the values
of vect, based upon minimization of deviation between actual epidemics data
and observed generation time. The optimized function is \code{\link[=censored.deviation]{censored.deviation()}},
which returns the deviation used for minimization. Stand-alone use can be
conducted, however this assumes data are all of the correct format.
}
\author{
Pierre-Yves Boelle, Thomas Obadia
}
