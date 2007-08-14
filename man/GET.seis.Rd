\name{GET.seis}
\alias{GET.seis}
\alias{testgetseis}
\title{Reads various seismic file formats}
\description{
  This fuction calls c-routines to read in \sQuote{segy}, \sQuote{sac},
  \sQuote{AH} formats.
}
\usage{
GET.seis(fnames, kind = 1, PLOT = FALSE)
testgetseis(GG)
}
\arguments{
  \item{fnames}{list of file names.}
  \item{kind}{an integer {1, 2, 3}; 1 = segy, 2 = sac, 3 = AH.}
  \item{PLOT}{logical, for if the traces should be plotted.}
  \item{GG}{a list returned by \code{GET.seis}.}
}
\value{a list containing the seismic data and header information.
}
\seealso{\code{\link{rsac}}
}
\author{Jonathan M. Lees <jonathan.lees@unc.edu>}
\examples{


}
\keyword{}
