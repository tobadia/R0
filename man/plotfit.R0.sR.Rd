\name{plotfit.R0.sR}
\alias{plotfit.R0.sR}
\title{Plot the fit of all requested models to epidemic data}
\description{Plots the fit of all requested models to epidemic data}
\usage{\method{plotfit}{R0.sR}(x, all = TRUE, xscale = "w", SB.dist = TRUE, ...)}
\arguments{
  \item{x}{Result of est.R (class R0.R)}
  \item{all}{Should the whole epidemic curve be shown}
  \item{xscale}{Scale to be adjusted on X axis. Can be "d" (day), "w" (week (default)), "f" (fornight), "m" (month).}
  \item{SB.dist}{Should R distribution throughout the epidemic be plotted for SB method? (default: TRUE)}
  \item{\dots}{Parameters passed to plot}
}
\details{For internal use. Called by plotfit.

Tweaked plot() function that draws the epidemic data and model fit of each method contained in the object constructed by est.RO().}
\value{Called for its side effect :
Draws all the epidemic curves and associated fit data computed by est.R0}

\author{Pierre-Yves Boelle, Thomas Obadia}






\keyword{internal}
