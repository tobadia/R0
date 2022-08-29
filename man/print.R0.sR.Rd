\name{print.R0.sR}
\alias{print.R0.sR}
\title{Plot the R0/Rt value along with confidence interval of all requested models to epidemic data}
\description{Plot the R0/Rt value along with confidence interval of all requested models to epidemic data}
\usage{\method{print}{R0.sR}(x, ...)}
\arguments{
  \item{x}{Result of est.R0 (class sR))}
  \item{\dots}{Parameters passed to inner functions}
}
\details{For internal use. Called by print.

Tweaked print() function that prints the reproduction number values for each method contained in the object constructed by est.RO().}
\value{Called for its side effect :
Prints all R0 or R(t) values from requested estimation methods.}

\author{Pierre-Yves Boelle, Thomas Obadia}






\keyword{internal}
