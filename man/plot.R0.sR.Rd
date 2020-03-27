\name{plot.R0.sR}
\alias{plot.R0.sR}
\title{Plot the R0/Rt value along with confidence interval of all requested models to epidemic data}
\description{Plot the R0/Rt value along with confidence interval of all requested models to epidemic data}
\usage{\method{plot}{R0.sR}(x, xscale = "w", TD.split = FALSE, ...)}
\arguments{
  \item{x}{Result of est.R0 (class sR)}
  \item{xscale}{Scale to be adjusted on X axis. Can be "d" (day), "w" (week (default)), "f" (fornight), "m" (month).}
  \item{TD.split}{Parameter to force the display of both R(t) and the epidemic curve in the same window for TD method}
  \item{\dots}{parameters passed to plot.R}
}
\details{For internal use. Called by plot.

Tweaked plot() function that draws the reproduction number values for each method contained in the object constructed by est.RO().}
\value{Called for its side effect :
Draws all R0 or R(t) values from requested estimation methods.}

\author{Pierre-Yves Boelle, Thomas Obadia}






\keyword{internal}
