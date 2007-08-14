\name{tf}
\alias{tf}
\title{Transfer Function}
\description{
  Calculates the poles, zeros, and gain of the low-pass n-th order
  normalize prototype filter. Optionally displays the magnitude and
  phase response. Supported values of \code{type} are "BU" Butterworth,
  "BE" Bessel, "C1" Chebyshev type I, "C2" Chebyshev type II. The
  maximum supported order of a Bessel filter is 25.
}
\usage{
tf(type = "BU", n = 1, rp = NULL, rs = NULL, plots = TRUE, dB = FALSE)
}
\arguments{
  \item{type}{filter type.}
  \item{n}{filter order.}
  \item{rp}{decibels of ripple in the passband.}
  \item{rs}{decibels of ripple in the stopband.}
  \item{plots}{logical value for if the magnitude and phase response
    should be plotted.}
  \item{dB}{if the magnitude should be plotted in dB (here
    \code{20*log10(abs(H(s)))}). In this case, the frequency axis is
    logarithmic and only positive frequency is shown.}
}
\seealso{\code{\link{pztransfer}}, \code{\link{butpz}},
  \code{\link{besselpz}}, \code{\link{cheb1pz}}, \code{\link{cheb2pz}}
}
\value{a list containing the poles, zeros, and gain of the transfer
  function.
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{
tf()

tf(n = 4)

tf(n = 8, type = "BE")

tf(n = 4, type = "C1", rp = 3, dB = TRUE)

tf(n = 5, type = "C2", rs = 20, dB = TRUE)

}
\keyword{}
