\name{plot.R0.S}
\alias{plot.R0.S}
\title{Plot objects from sensitivity.analysis}
\description{Plots objects from \code{\link{sensitivity.analysis}}}
\usage{\method{plot}{R0.S}(x, what = "heatmap", 
    time.step = 1, skip = 5, 
    ...)}
\arguments{
  \item{x}{Result of \code{\link{sensitivity.analysis}} (class R0.S)}
  \item{what}{Specify the desired output. Can be "heatmap" (default), "criterion", or both.}
  \item{time.step}{Optional. If date of first observation is specified, number of day between each incidence observation}
  \item{skip}{Number of results to ignore (time period of X days) when looking for highest Rsquared value.}
  \item{\dots}{Parameters passed to inner functions}
}
\details{For internal use. Called by plot.}
\value{A data frame with best R0 measure for each possible time period, along with corresponding begin/end dates
\item{$max.Rsquared}{Best R0 measure for each time period, as measured by their Rsquared value.}}

\author{Pierre-Yves Boelle, Thomas Obadia}





