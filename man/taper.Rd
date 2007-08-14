\name{taper}
\alias{taper}
\title{Apply a taper}
\description{
  Applies a symmetric taper to each side of SAC data.
}
\usage{
taper(x, type = c("hanning", "hamming", "cosine"),
      width = 0.05)
}
\arguments{
  \item{x}{an object of class \code{"rsac"}.}
  \item{type}{Valid values are: \sQuote{hanning}, \sQuote{hamming}, and
    \sQuote{cosine}.} 
  \item{width}{the width of the taper. Values larger than 0.5 should
    probably never be used.}
}
\value{same as input.
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

}
\keyword{}
