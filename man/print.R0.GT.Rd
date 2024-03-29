% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R0.GT.R
\name{print.R0.GT}
\alias{print.R0.GT}
\title{Print method for objects of class \code{R0.GT}}
\usage{
\method{print}{R0.GT}(x, ...)
}
\arguments{
\item{...}{Parameters passed to inner functions.}

\item{GT}{Generation time distribution from \code{\link[=generation.time]{generation.time()}}.}
}
\value{
This function does not return any data.
Called for side effects.
}
\description{
Prints summary statistics for a generation time distribution from
\code{\link[=generation.time]{generation.time()}}.
}
\details{
For internal use.

Displays the mean and standard deviation for generation time distributions
created with \code{\link[=generation.time]{generation.time()}}, along with the probabilities for each
time interval.
}
\author{
Pierre-Yves Boelle, Thomas Obadia
}
\keyword{internal}
