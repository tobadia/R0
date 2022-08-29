\name{sim.epid}
\alias{sim.epid}
\title{Epidemic outbreak simulation}
\description{Generates several epidemic curves with specified distribution and reproduction number.}
\usage{sim.epid(epid.nb, GT, 
    R0, epid.length, 
    family, negbin.size = NULL, 
    peak.value = 50)}
\arguments{
  \item{epid.nb}{Number of outbreaks to be generated.}
  \item{GT}{Generation time distribution for the pathogen. Must be a R0.GT-class object.}
  \item{R0}{Basic reproduction number.}
  \item{epid.length}{Length of the epidemic.}
  \item{family}{Distribution type for the new cases, either "poisson" or "negbin".}
  \item{negbin.size}{Over-dispersion parameter, if \code{family} is set to "negbin".}
  \item{peak.value}{Threashold value for incidence before epidemics begins decreasing}
}
\details{This function is only used for simulation purposes. The output is a matrix of n columns (number of outbreaks)
by m rows (maximum length of an outbreak).

When using rnbinom with "mean" and "size" moments, the variance is given by mean + mean^2/size (see ?rnbinom).
One should determine the size accordingly to the \code{R0} value to increase the dispersion.
From the previous variance formula, if Var(X) = k*R0, size = R0/(k-1)}


\author{Pierre-Yves Boelle, Thomas Obadia}




\examples{#Loading package
library(R0)

## In this example we simulate n=100 epidemic curves, with peak value at 150 incident cases, 
## and maximum epidemic length of 30 time units.
## Only the outbreak phase is computed. When the peak value is reached, the process is stopped 
## and another epidemic is generated.
sim.epid(epid.nb=100, GT=generation.time("gamma",c(3,1.5)), R0=1.5, 
         epid.length=30, family="poisson", peak.value=150)

# Here, a 30*100 matrix is returned. Each column is a single epidemic.}
