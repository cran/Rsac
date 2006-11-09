\name{dif}
\alias{dif}
\title{Differentiation}
\description{
  Differentiation in the frequency or time (with two-point differencing)
  domain. In the time domain the number of points decreses by one and 
  the begin time of the time series is increased by half the sampling
  interval.
}
\usage{
dif(s, domain = "f")
}
\arguments{
  \item{s}{a vector of SAC data.}
  \item{domain}{\dQuote{f} for frequency domain; \dQuote{t} for time domain.}
}
\value{a vector of same length as input.
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

data(srosa)
getunits(srosa)
# convert to cm/s
srosa.accel <- dif(mul(srosa, 1e-7))

amax <-  function(X) X$x <- max(abs(X$x))

am <- max(sapply(X = srosa.accel, amax))
# cm/s/s
}
\keyword{}
