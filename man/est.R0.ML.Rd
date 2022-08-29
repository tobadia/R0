\name{est.R0.ML}
\alias{est.R0.ML}
\title{Estimate the reproduction number by maximum likelihood}
\description{Estimate the reproduction number by maximum likelihood}
\usage{est.R0.ML(epid, GT, import = NULL, 
    t = NULL, begin = NULL, 
    end = NULL, date.first.obs = NULL, 
    time.step = 1, range = c(0.01, 
        50), unknown.GT = FALSE, 
    impute.incid = FALSE, 
    checked = FALSE, 
    ...)}
\arguments{
  \item{epid}{the epidemic curve}
  \item{GT}{generation time distribution}
  \item{import}{Vector of imported cases.}
  \item{t}{Vector of dates at which incidence was calculated}
  \item{begin}{At what time estimation begins}
  \item{end}{Time at which to \code{end} computation}
  \item{date.first.obs}{Optional date of first observation, if \code{t} not specified}
  \item{time.step}{Optional. If date of first observation is specified, number of day between each incidence observation}
  \item{range}{Range in which the maximum must be looked for}
  \item{unknown.GT}{When \code{GT} distribution is unknown, it is estimated jointly. See details.}
  \item{impute.incid}{Boolean value. If TRUE, will impute unobserved cases at the beginning of the epidemic to correct for censored data}
  \item{checked}{Internal flag used to check whether integrity checks were ran or not.}
  \item{\dots}{parameters passed to inner functions}
}
\details{For internal use. Called by est.R0.

White & Pagano (2009) detail two maximum likelihood methods for estimatig the reproduction ratio.
The first (and used by default in this package) assumes that the serial interval distirbution is known, and subsequently
the likelihood is only maximised depending on the value of R.
The second method can be used if the serial interval distribution is unknown: in that case, the generation time is set to
follow a Gamma distribution with two parameters (size, shape), and the optimization routine finds the values of R, size and shape
that maximize the likelihood. However, the epidemic curve must be long enough to account for a whole generation. The authors showed
that this is achieved when the cumulated amount of incident cases reaches 150. 
When using this method, the flag \code{unknown.GT} must be set to \code{TRUE}. \code{GT} must still be provided with a R0.GT-class object, however its mean and sd will be recycled as starting value for the optimization routine.

The principle of the methods described by White & all is to compute the expected number of 
cases in the future, and optimise to get R using a Poisson distribution.

CI is achieved by profiling the likelihood.}
\value{A list with components:
\item{R}{The estimate of the reproduction ratio.}
\item{conf.int}{The 95\% confidence interval for the R estimate.}
\item{epid}{Original or augmented epidemic data, depending whether \link{\code{impute.incid}} is set to FALSE or TRUE.}
\item{epid.orig}{Original epidemic data.}
\item{GT}{Generation time distribution uised in the computation.}
\item{begin}{Starting date for the fit.}
\item{begin.nb}{The number of the first day used in the fit.}
\item{end}{The \code{end} date for the fit.}
\item{end.nb}{The number of the las day used for the fit.}
\item{pred}{Prediction on the period used for the fit.}
\item{Rsquared}{Correlation coefficient between predicted curve (by \code{\link{fit.epid}}) and observed epidemic curve.}
\item{call}{Call used for the function.}
\item{method}{Method used for fitting.}
\item{method.code}{Internal code used to designate method.}}
\references{White, L.F., J. Wallinga, L. Finelli, C. Reed, S. Riley, M. Lipsitch, and M. Pagano. "Estimation of the Reproductive Number and the Serial Interval in Early Phase of the 2009 Influenza A/H1N1 Pandemic in the USA." Influenza and Other Respiratory Viruses 3, no. 6 (2009): 267-276.}
\author{Pierre-Yves Boelle, Thomas Obadia}
\note{This is the implementation of the method provided by White & Pagano (2009).}



\examples{#Loading package
library(R0)

## Data is taken from paper by Nishiura for key transmission parameters of an institutional
## outbreak during the 1918 influenza pandemic in Germany)

data(Germany.1918)
mGT<-generation.time("gamma", c(2.45, 1.38))
est.R0.ML(Germany.1918, mGT, begin=1, end=27, range=c(0.01,50))
# Reproduction number estimate using  Maximum Likelihood  method.
# R :  1.307222[ 1.236913 , 1.380156 ]

res=est.R0.ML(Germany.1918, mGT, begin=1, end=27, range=c(0.01,50))
plot(res)

## no change in R with varying range 
## (dates here are the same index as before. Just to illustrate different use)
est.R0.ML(Germany.1918, mGT, begin="1918-09-29", end="1918-10-25", range=c(0.01,100))
# Reproduction number estimate using  Maximum Likelihood  method.
# R :  1.307249[ 1.236913 , 1.380185 ]

}
