\name{Poles and zeros}
\alias{buttap}
\alias{besselap}
\alias{cheb1ap}
\alias{cheb2ap}
\title{Calculate poles, zeros, and gain for various filters.}
\description{
  \code{buttap} returns the poles, zeros, and gain of the n-th order
  normalized prototype Butterworth analog lowpass filter;
  \code{besselap} returns the poles, zeros, and gain of the n-th order
  normalized prototype Bessel analog lowpass filter; The maximum order
  supported for Bessel filters is 25.
  \code{cheb1ap} returns the poles, zeros, and gain of the n-th order
  normalized analog prototype Chebyshev Type I lowpass filter with rp
  decibels of ripple in the passband;
  \code{cheb2ap} returns the poles, zeros, and gain of the n-th order
  normalized analog prototype Chebyshev Type II lowpass filter with rp
  decibels of ripple in the passband.
}
\usage{
buttap(n)
besselap(n)
cheb1ap(n, rp)
cheb2ap(n, rs)
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
