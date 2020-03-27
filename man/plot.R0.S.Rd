\name{plot.R0.S}
\alias{plot.R0.S}
\title{Plot objects from sensitivity.analysis}
\description{Plots objects from sensitivity.analysis}
\usage{\method{plot}{R0.S}(x, what = "heatmap", time.step = 1, skip = 5, ...)}
\arguments{
  \item{x}{Result of sensitivity.analysis (class R0.S)}
  \item{what}{Specify the desired output. Can be "heatmap" (default), "criterion", or both.}
  \item{time.step}{Optional. If date of first observation is specified, number of day between each incidence observation}
  \item{skip}{Number of results to ignore (time period of X days) when looking for highest Rsquared value.}
  \item{\dots}{Parameters passed to inner functions}
}
\details{For internal use. Called by plot.}
\value{A data frame with best R0 measure for each possible time period, along with corresponding begin/end dates
\item{$max.Rsquared}{Best R0 measure for each time period, as measured by their Rsquared value.}}

\author{Pierre-Yves Boelle, Thomas Obadia}




\examples{\dontrun{#Loading package
library(R0)

## Data is taken from the paper by Nishiura for key transmission parameters of an institutional
## outbreak during 1918 influenza pandemic in Germany

data(Germany.1918)
mGT<-generation.time("gamma", c(2.6,1))
## sensitivity analysis for begin between day 1 and 15, and end between day 16 and 30
sen = sensitivity.analysis(sa.type="time", incid=Germany.1918, GT=mGT, begin=1:15, end=16:30, 
                           est.method="EG")
# Waiting for profiling to be done...
# [...]
# Waiting for profiling to be done...
# Warning message:
# If 'begin' and 'end' overlap, cases where begin >= end are skipped.
# These cases often return Rsquared = 1 and are thus ignored. 


## Return data.frame which can be plotted. Provides the best Rsquared measures for each 
## time interval, along with a coloured matrix representing R0 values
## Return 2 plots, and also a list with max.Rsquared and best R0 values for each time period
plot(sen, what=c("criterion","heatmap"))

# $max.Rsquared
# [very big data.frame]
# 
# $best.fit
#     Time.period Begin.dates  End.dates       R Growth.rate  Rsquared CI.lower CI.upper
# 122          15  1918-01-07 1918-01-22 1.64098   0.1478316 0.9752564  1.574953  1.710209}
}
