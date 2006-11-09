\name{sync}
\alias{sync}
\title{Synchronize}
\description{
  Synchronizes the reference times of all files in a vector of SAC
  files. This is done automatically in \code{\link{psac}} when plotting
  all the data at once. This is useful if you are sending each file to a
  different plot but want them to be aligned.
}
\usage{
sync(s)
}
\arguments{
  \item{s}{a vector of SAC data.}
}
\value{a vector of same length as input.
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

data(srosa)
par(mfrow = c(3, 1), mar = c(3,4,1,1))
xlim = c(0, 400)
psac(srosa[1], axes = FALSE, ylab = "z",
     xlab = "", xlim = xlim)
axis(side = 2)
psac(srosa[2], axes = FALSE, ylab = "North-South",
     xlab = "", xlim = xlim)
axis(side = 2)
psac(srosa[3], axes = FALSE, ylab = "East-West",
     xlab = "Time", xlim = xlim)
axis(side = 2); axis(side = 1)

# Better:
srosa <- sync(srosa)
par(mfrow = c(3, 1), mar = c(3,4,1,1))
xlim = c(0, 400)
psac(srosa[1], axes = FALSE, ylab = "z",
     xlab = "", xlim = xlim)
axis(side = 2)
psac(srosa[2], axes = FALSE, ylab = "North-South",
     xlab = "", xlim = xlim)
axis(side = 2)
psac(srosa[3], axes = FALSE, ylab = "East-West",
     xlab = "Time", xlim = xlim)
axis(side = 2); axis(side = 1)



}
\keyword{}
