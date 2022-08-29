\name{check.incid}
\alias{check.incid}
\title{Check incid in the input}
\description{Checks \code{incid} in the input. For internal use only.}
\usage{check.incid(incid, t = NULL, 
    date.first.obs = NULL, 
    time.step = 1)}
\arguments{
  \item{incid}{An object (vector, data.frame, list) storing incidence}
  \item{t}{An optional vector of dates.}
  \item{date.first.obs}{Optional date of first observation, if \code{t} not specified}
  \item{time.step}{Optional. If date of first observation is specified, number of day between each incidence observation}
}
\details{For internal use. Called by estimation methods to format incidence input.

check.incid handles everything related to incidence content integrity. It is designed to generate an
output which comply with estimation functions requirements. Epidemic data can be provided as an epitools object (see below)
or as vectors (incidence, dates, or both).
When dates are provided, they can be in a separate \code{t} vector, or computed with the first value and a time step.
In the end, the function returns a list with "epid" and "t" values.
If you plan on using estimation functions on their own (and not throught est.R0), be aware that any incorrect input
format will result in erratic behavior and/or crash.

Object \code{incid} is either a list or data.frame. Expect item/column "$dates"" and/or "$stratum3". This is expected to work with objects created by epitools package (tested with v0.5-6).

Epicurve.dates returns (among other things) a list with $dates object. This list gives incidence per day.
Other epicurve methods return $dates along with a $<time_period> object and a $stratum3, which contains
respectively daily incidence data agregated by the given time period, and the same data with colnames
that comply with R standard time notation.

E.g.: epicurve.weeks returns $dates, $weeks and $stratum3. $stratum3 object is a list of dates (correct syntax), where each date is repeated to reflect the incidence value at this time.

Incidence data should not contain negative or missing values.

Incidence data and time vector should have the same length.}
\value{A list with components \code{incid} and \code{t}.}

\author{Pierre-Yves Boelle, Thomas Obadia}




\examples{#Loading package
library(R0)

## Data is taken from the paper by Nishiura for key transmission parameters of an institutional
## outbreak during 1918 influenza pandemic in Germany
data(Germany.1918)
Germany.1918

## check.incid will extract names from the vector and coerce them as dates
check.incid(Germany.1918)

## Had Germany.1918 not have names() set, output would have been with index dates
## To force such an output, we here impose t=1:126. 
## Erasing names(Germany.1918) would have produced the same
## If so, then the epid$t vector returned will be replacement values.
check.incid(Germany.1918, t=1:126)

## You can also choose not to provide a complete date vector, but to only
## indicated the first day of the observation, and the number of days between each
## observation. In this example we will assume a time step of 7 days.
check.incid(Germany.1918, date.first.obs="1918-01-01", time.step=7)

## Finally, if no names() are available for the dataset and date.first.obs is not provided,
## setting time.step to any integer value will generate a t vector starting 
## from 1 and incrementing by the time.step parameter.}
