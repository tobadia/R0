\name{R.from.r}
\alias{R.from.r}
\title{Compute the discretized Laplace Transform using a discretized Generation Time distribution}
\description{Computes the discretized Laplace Transform using a discretized Generation Time distribution.}
\usage{R.from.r(r, GT)}
\arguments{
  \item{r}{exponential growth ratio}
  \item{GT}{discretized generation time distribution}
}
\details{For internal use. Called by \code{\link{est.R0.EG}}.}
\value{An R value corresponding to inverse of discretized Laplace transform.}

\author{Pierre-Yves Boelle, Thomas Obadia}
\note{The formula for the discretized Laplace transform is taken from Wallinga.}





\keyword{internal}
