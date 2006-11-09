\name{taper}
\alias{taper}
\title{Apply a taper}
\description{
  Applies a symmetric taper to each side of SAC data.
}
\usage{
taper(s, type = "hanning", width = 0.05)
}
\arguments{
  \item{s}{a vector of SAC data.}
  \item{type}{Valid values are: \dQuote{hanning}, \dQuote{hamming}, and
      \dQuote{cosine}.}
  \item{width}{the width of the taper. Values larger than 0.5 should
    probably never be used.}
}
\value{a vector of same length as input.
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

}
\keyword{}
