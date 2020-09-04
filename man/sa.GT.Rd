\name{sa.GT}
\alias{sa.GT}
\title{Sensitivity analysis of reproduction ratio with varying GT distribution}
\description{Sensitivity analysis of reproduction ratio with varying GT distribution.}
\usage{sa.GT(incid, GT.type, 
    GT.mean.range, GT.sd.range, 
    begin = NULL, end = NULL, 
    est.method, t = NULL, 
    date.first.obs = NULL, 
    time.step = 1, ...)}
\arguments{
  \item{incid}{incident cases}
  \item{GT.type}{Type of distribution for GT (see \code{\link{generation.time}} for details)}
  \item{GT.mean.range}{mean used for all GT distributions throughout the simulation}
  \item{GT.sd.range}{Range of standard deviation used for GT distributions. Must be provided as a vector.}
  \item{begin}{\code{begin} date of the estimation of epidemic}
  \item{end}{\code{end} date of estimation of the epidemic}
  \item{est.method}{Estimation method used for sensitivity analysis. Requires a method computing a proper R0 value (and not an instantaneous R(\code{t}))}
  \item{t}{Dates vector to be passed to estimation function}
  \item{date.first.obs}{Optional date of first observation, if \code{t} not specified}
  \item{time.step}{Optional. If date of first observation is specified, number of day between each incidence observation}
  \item{\dots}{parameters passed to inner functions}
}
\details{By using different Generation Time (GT) distribution, different estimates of reproduction ratio can be analyzed.}
\value{A data frame s.a with following components :
\item{$GT.type}{Distribution law for GT.}
\item{$GT.mean}{Range of means used for tested GTs.}
\item{$GT.sd}{Range of standard deviations used for tested GTs.}
\item{$R}{Computed value for Reproduction Number given \code{GT.type}, GT.mean and GT.sd.}
\item{$conf.int[1]}{The lower limit of 95\\% CI for R.}
\item{$conf.int[2]}{The upper limit of 95\\% CI for R.}}

\author{Pierre-Yves Boelle, Thomas Obadia}





