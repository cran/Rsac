\name{getunits}
\alias{getunits}
\title{SAC time series units}
\description{
  Looks up the units for when they are specified in the header as
  integer values. The units in many seismic headers are notoriously
  unreliable, so care should be taken to find an independent source
  to confirm the units. 
}
\usage{
getunits(x)
}
\arguments{
  \item{x}{an object of class \code{"rsac"}.}
}
\value{a character vector that describe the units of the SAC files.
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

}
\keyword{}
