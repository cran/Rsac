\name{mul}
\alias{mul}
\alias{add}
\alias{pow}
\title{Some simple arithmetic}
\description{
  These are provided as a matter of convience since any R functions can
  be easily applied to the vectors of SAC data (see below for
  examples). These functions apply basic arithmetic functions to each
  SAC data in a vector. The constant may be the same or different for
  each SAC file. If less constants are given than files in the vector,
  then the last constant provided is used for the remainder of the
  files, to be consistent with the behavior of the original SAC code.
}
\usage{
mul(s, c)
add(s, c)
pow(s, c)
}
\arguments{
  \item{s}{a vector of SAC data.}
  \item{c}{a constant to be applied to the data.}
}
\value{a vector of same length as input.
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

data(srosa)

getunits(srosa)
# Convert to m/s
srosa <- mul(srosa, 1e-9)
p(srosa, xlim = c(150, 160))

# Alternatively, for all files

m <- function(X){
       X$x <- X$x * 1e-9
       return(X)
     }
srosa <- lapply(X = srosa, m)

# Or individually:

srosa[[1]]$x <- abs(srosa[[1]]$x)

}
\keyword{}
