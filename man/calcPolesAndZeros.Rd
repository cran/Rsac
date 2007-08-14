\name{Poles and zeros}
\alias{butpz}
\alias{besselpz}
\alias{cheb1pz}
\alias{cheb2pz}
\title{Calculate poles, zeros, and gain for various filters.}
\description{
  \code{butpz} returns the poles, zeros, and gain of the n-th order
  Butterworth lowpass filter.
  \code{besselap} returns the poles, zeros, and gain of the n-th order
  Bessel lowpass filter; Maximum supported order is 25.
  \code{cheb1pz} returns the poles, zeros, and gain of the n-th order
  Chebyshev Type I lowpass filter with rp decibels of ripple in the
  passband.
  \code{cheb2pz} returns the poles, zeros, and gain of the n-th order
  Chebyshev Type II lowpass filter with rp decibels of ripple in the
  passband.
}
\usage{
butpz(n)
besselpz(n)
cheb1pz(n, rp)
cheb2pz(n, rs)
}
\arguments{
  \item{n}{filter order.}
  \item{rp}{decibels of ripple in the passband.}
  \item{rs}{decibels of ripple in the stopband.}
}
\seealso{\code{\link{tf}}, \code{\link{pztransfer}}
}
\value{none}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{
}
\keyword{}
