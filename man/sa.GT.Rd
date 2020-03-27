\name{sa.GT}
\alias{sa.GT}
\title{Sensitivity analysis of reproduction ratio with varying GT distribution}
\description{Sensitivity analysis of reproduction ratio with varying GT distribution.}
\usage{sa.GT(incid, GT.type, GT.mean.range, GT.sd.range, begin = NULL, 
    end = NULL, est.method, t = NULL, date.first.obs = NULL, 
    time.step = 1, ...)}
\arguments{
  \item{incid}{incident cases}
  \item{GT.type}{Type of distribution for GT (see GT.R for details)}
  \item{GT.mean.range}{mean used for all GT distributions throughout the simulation}
  \item{GT.sd.range}{Range of standard deviation used for GT distributions. Must be provided as a vector.}
  \item{begin}{begin date of the estimation of epidemic}
  \item{end}{end date of estimation of the epidemic}
  \item{est.method}{Estimation method used for sensitivity analysis. Requires a method computing a proper R0 value (and not an instantaneous R(t))}
  \item{t}{Dates vector to be passed to estimation function}
  \item{date.first.obs}{Optional date of first observation, if t not specified}
  \item{time.step}{Optional. If date of first observation is specified, number of day between each incidence observation}
  \item{\dots}{parameters passed to inner functions}
}
\details{By using different Generation Time (GT) distribution, different estimates of reproduction ratio can be analyzed.}
\value{A data frame s.a with following components :
\item{$GT.type}{Distribution law for GT.}
\item{$GT.mean}{Range of means used for tested GTs.}
\item{$GT.sd}{Range of standard deviations used for tested GTs.}
\item{$R}{Computed value for Reproduction Number given GT.type, GT.mean and GT.sd.}
\item{$conf.int[1]}{The lower limit of 95\% CI for R.}
\item{$conf.int[2]}{The upper limit of 95\% CI for R.}}

\author{Pierre-Yves Boelle, Thomas Obadia}




\examples{\dontrun{#Loading package
library(R0)

## Data is taken from the paper by Nishiura for key transmission parameters of an institutional
## outbreak during 1918 influenza pandemic in Germany)
## Here we will test GT with means of 1 to 5, each time with SD constant (1)
## GT and SD can be either fixed value or vectors of values
## Actual value in simulations may differ, as they are adapted according to the distribution type
data(Germany.1918)
tmp<-sa.GT(incid=Germany.1918, GT.type="gamma", GT.mean=seq(1,5,1), GT.sd.range=1, begin=1, end=27,
           est.method="EG")

## Results are stored in a matrix, each line dedicated to a (mean,sd) couple
plot(x=tmp[,"GT.Mean"], xlab="mean GT (days)", y=tmp[,"R"], ylim=c(1.2, 2.1), ylab="R0 (95% CI)",
     type="p", pch=19, col="black", main="Sensitivity of R0 to mean GT")
arrows(x0=as.numeric(tmp[,"GT.Mean"]), y0=as.numeric(tmp[,"CI.lower"]), 
       y1=as.numeric(tmp[,"CI.upper"]), angle=90, code=3, col="black", length=0.05)

## One could tweak this example to change sorting of values (per mean, or per standard deviation)
## eg: 'x=tmp[,c('GT.Mean')]' could become 'x=tmp[,c('GT.SD')]'}
}
