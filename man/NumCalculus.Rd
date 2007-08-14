\name{Calculus}
\alias{dif}
\alias{int}
\title{Differentiation and Integration}
\description{
  Differentiation (by two-point differencing) and integration (by
  rectangular approximation) in the time domain. For differentiation,
  the begining time of the time series is increased by half the sampling
  interval, and the length of the record is decreased by 1. It is also
  usually a good idea to use the \code{\link{rtrend}} or
  \code{\link{rmean}} function before integration.
}
\usage{
dif(s)
int(s)
}
\arguments{
  \item{s}{a vector of SAC data.}
}
\value{a vector of same length as input.
}
\seealso{\code{\link{rtrend}}}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

data(srosa)
getunits(srosa)
# convert to cm/s
srosa.accel <- dif(mul(srosa, 1e-7))

amax <-  function(X) X$x <- max(abs(X$x))

am <- max(sapply(X = srosa.accel, amax))
# cm/s/s

spect(srosa, type = "l", log = "y")

srosa.disp <- int(rtrend(srosa))

p(srosa, xlim = c(100, 400))
p(srosa.disp, xlim = c(100, 400))
srosa.disp <- hp(srosa.disp, c = 0.1, n = 4, p = 2)
p(srosa.disp, xlim = c(100, 400))
spect(srosa.disp, type = "l", log = "y",
      ylim = c(1e-2, 1e2)
     )
}
\keyword{}
