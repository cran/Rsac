\name{psac}
\alias{psac}
\title{Plot in same graph}
\description{
  Plots all the time series of each SAC file in the plot. If
  unspecified, the lty and col are automatically incremented for each
  trace. 
} 
\usage{
psac(s, ylim = NULL, lty = NULL, col = NULL, \ldots)
}
\arguments{
  \item{s}{a vector of SAC data.}
  \item{ylim}{y-axis limits.}
  \item{lty}{line type.}
  \item{col}{line color.}
  \item{\ldots}{further graphical parameters. See \code{plot.default}
    in base package.}
}
\value{none.
}
\seealso{\code{\link{p}}, \code{\link{spect}}
}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

data(srosa)

srosa[[1]]$idep

psac(srosa)

p(srosa)
p(srosa, xlim = c(140, 280))

getunits(srosa)

# Not so good:
par(mfrow = c(3, 1), mar = c(3,4,1,1))
xlim <- c(140, 240)
psac(srosa[1], axes = FALSE, ylab = "z",
     xlab = "", xlim = xlim)
axis(side = 2)
psac(srosa[2], axes = FALSE, ylab = "North-South",
     xlab = "", xlim = xlim)
axis(side = 2)
psac(srosa[3], axes = FALSE, ylab = "East-West",
     xlab = "Time", xlim = xlim)
axis(side = 2); axis(side = 1)

# Need sync():

srosa <- sync(srosa)
par(mfrow = c(3, 1), mar = c(3,4,1,1))
xlim <- c(140, 240)
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
