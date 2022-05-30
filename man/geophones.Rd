\name{geophones}
\alias{geophones}
\docType{data}
\title{Seismic Timing Data}
\usage{geophones}
\description{
The \code{geophones} data frame has 56 rows and 2 columns.
Thickness of a layer of Alberta substratum as measured by
a line of geophones.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{distance}{location of geophone.}
    \item{thickness}{time for signal to pass through substratum.}
  }
}
\examples{
plot(geophones)
lines(lowess(geophones, f=.25))
}
\keyword{datasets}
