\name{spect}
\alias{spect}
\title{Plot the power spectrum}
\description{
  Plots the power spectrum of each SAC file.
}
\usage{
spect(s, log = "", \ldots)
}
\arguments{
  \item{s}{a vector of SAC data.}
  \item{log}{logarithmeic axes.}
  \item{\ldots}{further graphical parameters. See \code{plot.default}.}
}
\value{an \code{invible} list with the following elements:
  \item{f}{frequency.}
  \item{MX}{power.}
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\seealso{\code{\link{p}}, \code{\link{psac}}}
\examples{

data(srosa)
spect(srosa, type = 'l')
srosa.hp <- hp(srosa, c = 0.01, n = 3, p = 2)
spect(srosa.hp, type = 'l')

spect(srosa.hp, type = 'l', log = "y")

}
\keyword{}
