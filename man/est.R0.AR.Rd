\name{est.R0.AR}
\alias{est.R0.AR}
\title{Estimate R0 from attack rate of an epidemic}
\description{Estimate R0 from attack rate of an epidemic.}
\usage{est.R0.AR(AR = NULL, incid = NULL, pop.size = NULL, S0 = 1, checked = FALSE, 
    ...)}
\arguments{
  \item{AR}{Attack rate as a percentage from total population}
  \item{incid}{Sum of incident cases, possibly in the form of a vector of counts.}
  \item{pop.size}{Population size in which the incident cases were observed.}
  \item{S0}{Initial proportion of the population considered susceptible.}
  \item{checked}{Internal flag used to check whether integrity checks were ran or not.}
  \item{\dots}{parameters passed to inner functions}
}
\details{For internal use. Called by est.R0.

In the simple SIR model, the relation between R0 and the Attack Rate is in the form \eqn{R0 = -ln((1-AR)/S0) / (AR - (1-S0))}.

If the population size is provided, the variance of R0 is estimated using the delta method. 
The hypothesis are that of homogeneous mixing, no more transmission (epidemic ended), no change in transmission or interventions during the epidemic. This estimate may be correct in closed populations, and may be less valid in other cases.

The correction for incomplete susceptibility is based on the SIR model equations.

CI is computed for the attack rate considering the population size (\eqn{CI(AR) = AR +/- 1.96*sqrt(AR*(1-AR)/n)}), 
and so the CI for the reproduction number is computed with this extreme values.}
\value{A list with components:
\item{epid}{The vector of incidence, after being correctly formated by check.incid. Used only by plot.fit.}
\item{R}{The estimate of the reproduction ratio.}
\item{conf.int}{The 95\% confidence interval for the R estimate.}
\item{AR}{Attack rate as a percentage from total population}
\item{begin.nb}{First date of incidence record. Used only by plot.fit.}
\item{end.nb}{Last date of incidence record. Used only by plot.fit.}
\item{method}{Method used for the estimation.}
\item{method.code}{Internal code used to designate method.}}
\references{Dietz, K. "The Estimation of the Basic Reproduction Number for Infectious Diseases." Statistical Methods in Medical Research 2, no. 1 (March 1, 1993): 23-41.}
\author{Pierre-Yves Boelle, Thomas Obadia}
\note{This is the implementation of the formula by Dietz (1993).}



\examples{#Loading package
library(R0)

## Woodall reported an attack rate of 0.31 in a population of 1732 during
## the 1957 H2N2 influenza pandemic ('Age and Asian Influenza, 1957', BMJ, 1958)

est.R0.AR(pop.size=1732, AR=0.31)
# Reproduction number estimate using Attack Rate method
# R :  1.19698[ 1.179606 , 1.215077 ]

est.R0.AR(AR=0.31)
# Reproduction number estimate using  Attack Rate  method.
# R :  1.19698

est.R0.AR(pop.size=1732, incid=31)
# Reproduction number estimate using Attack Rate method
# R :  1.009057[ 1.005873 , 1.012269 ]

est.R0.AR(pop.size=1732, incid=c(2,3,4,7,4,2,4,5))
# Reproduction number estimate using Attack Rate method
# R :  1.009057[ 1.005873 , 1.012269 ]

est.R0.AR(pop.size=1732, incid=c(2,3,0,7,4,2,0,5))
# Reproduction number estimate using Attack Rate method
# R :  1.006699[ 1.003965 , 1.009453 ]}
