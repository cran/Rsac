\name{srosa}
\docType{data}
\alias{srosa}
\title{Example SAC data}
\description{Example SAC data containing 3 components of motion.
}
\usage{
data(srosa)
}
\format{Vector containing 3 SAC files. Each is a different component of
  the seismogram. 
} 
\source{\url{http://www.ncedc.org/ncedc/station.info.html}}
\section{Background}{A 1999 earthquake in California recorded by BRK
  station of the BK network.
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\keyword{datasets}
\examples{

data(srosa)

names(srosa[[1]])

}
