\name{int}
\alias{int}
\title{Integration}
\description{
  Integration in the time (with rectangular approximation) or frequency
  domain. It is usually a good idea to use the \code{\link{rtrend}} function
  before integration.
}
\usage{
int(s, domain = "f")
}
\arguments{
  \item{s}{a vector of SAC data.}
  \item{domain}{\dQuote{f} for frequency domain; \dQuote{t} for time domain.}
}
\value{a vector of same length as input.
}
\seealso{\code{\link{rtrend}}}
\author{Eric M. Thompson <eric.thompson@tufts.edu>}
\examples{

data(srosa)

spect(srosa, type = "l", log = "y")

srosa.disp <- int(rtrend(srosa))

p(srosa, xlim = c(100, 400))
p(srosa.disp, xlim = c(100, 400))
srosa.disp <- hp(srosa.disp, c = 0.1, n = 4, p = 2)
p(srosa.disp, xlim = c(100, 400))
spect(srosa.disp, type = "l", log = "y",
      ylim = c(1e-2, 1e2)
     )

}
\keyword{}
