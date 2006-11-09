\name{pztransfer}
\alias{pztransfer}
\alias{pz.tf}
\title{Pole-zero transfer}
\description{
  \code{pz.tf} calculates and \code{pztransfer} applies a transfer
  funciton from user-specified poles and zeros. Can be read from a file
  or set at the command prompt.
}
\usage{
pztransfer(s, file = NULL, p = NULL, z = NULL, g = NULL,
           domain = "t", decon = FALSE)
pz.tf(p = NULL, z = NULL, g = NULL)
}
\arguments{
  \item{s}{a vector of SAC data.}
  \item{file}{name of a \sQuote{pole-zero} file.}
  \item{p}{array of poles.}
  \item{z}{array of zeros.}
  \item{g}{gain.}
  \item{domain}{\dQuote{f} for frequency domain calculation; \dQuote{t} for time
    domain calculation.}
  \item{decon}{if FALSE, then the transfer function is applied TO the
    data in the vector \code{s}. If TRUE, then the transfer function
    response is removed FROM the data by spectral division in the
    frequency domain.}
}
\value{\code{pztransfer}: a vector of SAC data the same length as
input; \code{pz.tf}: the transfer function \code{H(s)}.
}
\seealso{\code{\link{lp}}, \code{\link{hp}}, \code{\link{buttap}},
  \code{\link{besselap}}, \code{\link{cheb1ap}}, \code{\link{cheb2ap}}
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

bu2 <- tf(n = 2)

H <- pz.tf(p = bu2$p)

H

}
\keyword{}
