\name{impute.incid}
\alias{impute.incid}
\title{Optimiziation routine for incidence imputation}
\description{When first records of incidence are unavailable, tries to impute censored cases to rebuild longer epidemic vector}
\usage{impute.incid(CD.optim.vect, 
    CD.epid, CD.R0, CD.GT)}
\arguments{
  \item{CD.optim.vect}{Vector of two elements (multiplicative factor, log(highest imputed data) to be optimized}
  \item{CD.epid}{Original epidemic vector, output of check.incid()}
  \item{CD.R0}{Assumed R0 value for the original epidemic vector}
  \item{CD.GT}{Generation time distribution to be used for computations}
}
\details{This function is not intended for stand-alone use. It optimizes the values of
vect, based upon minimization of deviation between actual epidemics data and observed generation time.
The optimized function is \code{\link{censored.deviation}}, which returns the deviation used for minimization.
Stand-alone use can be conducted, however this assumes data are all of the correct format. }
\value{A vector with both imputed incidence and source available data.}

\author{Pierre-Yves Boelle, Thomas Obadia}





