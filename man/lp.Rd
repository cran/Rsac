\name{lp}
\alias{lp}
\alias{hp}
\title{Low/High pass filter}
\description{
  Applies a low (or high) pass filter to the SAC data. If the number of
  passes is 2, then the calculation is always done in the frequency domain
  because it is identical to the time domain caclulation but faster.
}
\usage{
lp(s, c = 2, n = 1, p = 2, type = "BU", domain = "t")
hp(s, c = 2, n = 1, p = 2, type = "BU", domain = "t")
}
\arguments{
  \item{s}{a vector of SAC data.}
  \item{c}{the corner frequency.}
  \item{n}{filter order.}
  \item{p}{number of passes. Only supported values are 1 and 2, but it will
    treat value that is not 1 as 2.}
  \item{type}{filter type. see \code{\link{tf}}}
  \item{domain}{"f" for frequency domain; "t" for time domain.}
}
\value{a vector of SAC data the same length as input.
}
\seealso{\code{\link{tf}}, \code{\link{pz.tf}},
  \code{\link{butpz}}, \code{\link{besselpz}}, \code{\link{cheb1pz}},
  \code{\link{cheb2pz}}, \code{\link{taper}}
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

data(srosa)
p(srosa, xlim = c(100, 500))
tf(n = 4)
srosa.lp <- lp(taper(rtrend(srosa)), c = 0.2, n = 4, p = 2)
p(srosa.lp, xlim = c(100, 500))

}
\keyword{}
