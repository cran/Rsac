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
  \item{\code{C}:}{azimuth, in degrees.}
  \item{\code{B}:}{back azimuth, in degrees.}
  \item{\code{a}:}{arc distance, in degrees.}
  \item{\code{lat}:}{latitude defining the path.}
  \item{\code{lon}:}{longitude defining the path.}
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\references{
  Lay, T. and T. C. Wallace, (1995) \emph{Modern Global Seismology},
  International geophysics series; v. 58, San Diego: Academic Press.
}
\examples{

}
\keyword{internal}
