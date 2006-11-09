\name{gcp}
\alias{gcp}
\title{Great-circle-path parameters}
\description{
  Calculates the azimuth and back azimuth of the great-circle-path
  between two points (source and receiver).
}
\usage{
gcp(s, e)
}
\arguments{
  \item{s}{receiver, or station. This is a list with elements \code{lat}
    and \code{lon} given in degrees.
    }
  \item{e}{epicenter, or source. This is a list with elements \code{lat}
    and \code{lon} given in degrees.
    }
}
\value{a list containing the following elements:
  \item{C:}{azimuth, in degrees.}
  \item{B:}{back azimuth, in degrees.}
  \item{a:}{arc distance, in degrees.}
  \item{lat:}{array of latitudes defining the path.}
  \item{lon:}{array of longitudes defining the path.}
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\seealso{\code{\link{rotate}}}
\examples{

}
\keyword{internal}
