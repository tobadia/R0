\name{plotfit}
\alias{plotfit}
\title{Generic S3 method to plot either "R0.R" and "R0.sR" objects}
\description{Generic S3 method to plot either "R0.R" and "R0.sR" objects}
\usage{plotfit(x, all = TRUE, 
    xscale = "w", SB.dist = TRUE, 
    ...)}
\arguments{
  \item{x}{Object for which the fit should be plotted.}
  \item{all}{Should the whole epidemic curve be shown}
  \item{xscale}{Scale to be adjusted on X axis. Can be "d" (day), "w" (week (default)), "f" (fornight), "m" (month).}
  \item{SB.dist}{Should R distribution throughout the epidemic be plotted for SB method? (default: TRUE)}
  \item{\dots}{parameters passed to plot.R}
}
\details{plot.fit is designed to either call plot.fit.R0.R or plot.fit.R0.sR.
This S3 Method allows for plottinf the goodness of fit of a model to the original epidemic curve provided by user.
Depending on the method of estimation, the graphical output will vary:
- EG, ML and TD methods will show the original epidemic curve, along with the best-fitting prediction model
- AR will only show the epidemic curve, since no actual model is computed
- RTB will display 9 density curves for the R distribution throughout the epidemic}


\author{Pierre-Yves Boelle, Thomas Obadia}





