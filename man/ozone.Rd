\name{ozone}
\alias{ozone}
\non_function{}
\title{Ozone Data}
\usage{ozone}
\description{
Monthly provisional mean total ozone (in Dobson units) at Halley Bay 
(approximately corrected to Bass-Paur).
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Year}{the year}
    \item{Aug}{August mean total ozone}
    \item{Sep}{September mean total ozone}
    \item{Oct}{October mean total ozone}
    \item{Nov}{November mean total ozone}
    \item{Dec}{December mean total ozone}
    \item{Jan}{January mean total ozone}
    \item{Feb}{February mean total ozone}
    \item{Mar}{March mean total ozone}
    \item{Apr}{April mean total ozone}
    \item{Annual}{Yearly mean total ozone}
  }
}
\source{
Shanklin, J. (2001) Ozone at Halley, Rothera and Vernadsky/Faraday.  

http://www.antarctica.ac.uk/met/jds/ozone/data/zoz5699.dat
}
\references{
Christie, M. (2000) The Ozone Layer: a Philosophy of Science Perspective.
Cambridge University Press.
}
\examples{
AnnualOzone <- ts(ozone$Annual, start=1956)
plot(AnnualOzone)
}
\keyword{datasets}
