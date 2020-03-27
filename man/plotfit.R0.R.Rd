\name{plotfit.R0.R}
\alias{plotfit.R0.R}
\title{Plot the fit of a model to epidemic data}
\description{Plots the fit of a model to epidemic data}
\usage{\method{plotfit}{R0.R}(x, all = TRUE, xscale = "w", SB.dist = TRUE, ...)}
\arguments{
  \item{x}{Result of est.R (class R0.R)}
  \item{all}{Should the whole epidemic curve be shown}
  \item{xscale}{Scale to be adjusted on X axis. Can be "d" (day), "w" (week (default)), "f" (fornight), "m" (month).}
  \item{SB.dist}{Should R distribution throughout the epidemic be plotted for SB method? (default: TRUE)}
  \item{\dots}{Parameters passed to plot}
}
\details{For internal use. Called by plotfit.

For internal use. Called by plotfit.R0.sR.}
\value{Called for its side effect :
Draws the fit of one estimation method to the data.}

\author{Pierre-Yves Boelle, Thomas Obadia}






\keyword{internal}
