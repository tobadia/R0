\name{est.R0.EG}
\alias{est.R0.EG}
\title{Estimate R from exponential growth rate}
\description{Estimate R from exponential growth rate.}
\usage{est.R0.EG(epid, GT, t = NULL, begin = NULL, end = NULL, date.first.obs = NULL, 
    time.step = 1, reg.met = "poisson", checked = FALSE, ...)}
\arguments{
  \item{epid}{object containing epidemic curve data. see Details.}
  \item{GT}{generation time distribution}
  \item{t}{Vector of dates at which incidence was calculated}
  \item{begin}{At what time estimation begins}
  \item{end}{Time at which to end computation}
  \item{date.first.obs}{Optional date of first observation, if t not specified}
  \item{time.step}{Optional. If date of first observation is specified, number of day between each incidence observation}
  \item{reg.met}{Regression method used. Default is "poisson" (for GLM), but can be forced to "linear".}
  \item{checked}{Internal flag used to check whether integrity checks were ran or not.}
  \item{\dots}{parameters passed to inner functions}
}
\details{For internal use. Called by est.R0.

method "poisson" uses Poisson regression of incidence.
method "linear" uses linear regression of log(incidence)

CI is computed from the 1/M(-r) formula using bounds on r from the Poisson regression.}
\value{A list with components:
\item{R}{The estimate of the reproduction ratio.}
\item{conf.int}{The 95\% confidence interval for the R estimate.}
\item{r}{Exponential growth rate of the epidemic.}
\item{conf.int.r}{Confidence interval of the exponential growth rate of the epidemic.}
\item{Rsquared}{The deviance R-squared measure for the considered dates and model.}
\item{epid}{object containing epidemic curve data. see Details.}
\item{GT}{generation time distribution}
\item{data.name}{Name of the data used in the fit.}
\item{begin}{At what time estimation begins}
\item{begin.nb}{The number of the first day used in the fit.}
\item{end}{Time at which to end computation}
\item{end.nb}{The number of the las day used for the fit.}
\item{fit}{Method used for fitting.}
\item{pred}{Prediction on the period used for the fit.}
\item{method}{Method for estimation.}
\item{method.code}{Internal code used to designate method.}}
\references{Wallinga, J., and M. Lipsitch. "How Generation Intervals Shape the Relationship Between Growth Rates and Reproductive Numbers." Proceedings of the Royal Society B: Biological Sciences 274, no. 1609 (2007): 599.}
\author{Pierre-Yves Boelle, Thomas Obadia}
\note{This is the implementation of the method provided by Wallinga & Lipsitch (2007).}



\examples{#Loading package
library(R0)

## Data is taken from the paper by Nishiura for key transmission parameters of an institutional
## outbreak during 1918 influenza pandemic in Germany)

data(Germany.1918)
mGT<-generation.time("gamma", c(3, 1.5))

est.R0.EG(Germany.1918, mGT, begin=1, end=27)
## Reproduction number estimate using  Exponential Growth 
## R :  1.525895[ 1.494984 , 1.557779 ]}
