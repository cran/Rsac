\name{p}
\alias{p}
\title{Plot separately}
\description{
  A function for quickly viewing all the traces in a vector of SAC
  files. This function plots each traces separately, whereas
  \code{\link{psac}} plots all traces on the same graph.
}
\usage{
p(s, ...)
}
\arguments{
  \item{s}{a vector of SAC data.}
  \item{...}{further graphical parameters. See \code{plot.default}.}
}
\value{none.
}
\seealso{\code{\link{psac}}, \code{\link{spect}}
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

data(srosa)

p(srosa, xlim = c(150, 200))


}
\keyword{}
