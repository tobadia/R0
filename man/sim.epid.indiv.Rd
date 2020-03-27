\name{sim.epid.indiv}
\alias{sim.epid.indiv}
\title{Influenza-like illness simulation (individual-based model)}
\description{Generates several epidemic curves on a individual-based model}
\usage{sim.epid.indiv(beta, Tmax, n = 1, family = "poisson", negbin.size = NULL)}
\arguments{
  \item{beta}{Contact rate in the SEIR model.}
  \item{Tmax}{Maximum length of the epidemic (cases infected after this length will be truncated).}
  \item{n}{Number of epidemics to be simulated (default is 1)}
  \item{family}{Distribution of offspring (default is "poisson").}
  \item{negbin.size}{If family is set to "negbin", sets the size parameter of the negative binomial distribution.}
}

\value{A matrix with epidemics stored as columns (incidence count)}

\author{Pierre-Yves Boelle, Thomas Obadia}
\note{This is not the final version. This is the exact function as used in the manuscript (Obadia et al., 2012).
It will be properly implemented to conform with other objects of the package in future releases.

The epidemic is simulated using a branching process, with infinite number of susceptibles to allow for exponential growth. 
The model used follows the Crump-Mode-Jagers description, with S/E/I/R description of the natural history.
Latent and infectious period follow parametrized Gamma distributions typical of influenza. 
An index case is first introduced, and offspring is sampled from a negative binomial distribution, with mean \eqn{beta*I} and variance \eqn{negbin.size*beta*I}, to allow for overdispersion.}




