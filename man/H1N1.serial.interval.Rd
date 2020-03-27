\name{H1N1.serial.interval}
\alias{H1N1.serial.interval}
\docType{data}
\title{
H1N1 serial interval sample
}
\description{
Data taken from traced cases of H1N1 viruses.
}
\usage{data(H1N1.serial.interval)}
\format{
  The format is:
 num [1:355] 1 1 3 2 1 2 1 3 2 4 ...
}
\details{
Vector of values that represents the time lag between symptoms onset for pairs of infector/infectee, for a dataset of complete traced cases. Each value accounts for a pair of infector/infectee.
This serial interval is often substitued for the generation time distribution, as it is easier to observe.
}
\examples{
data(H1N1.serial.interval)
## maybe str(H1N1.serial.interval) ; plot(H1N1.serial.interval) ...
}
\keyword{datasets}
