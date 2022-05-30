\name{fossilfuel}
\alias{fossilfuel}
\docType{data}
\title{Fossil Fuel Data}
\usage{fossilfuel}
\description{Estimates of total worldwide carbon emissions from
fossil fuel use.
}
\details{Data for the years 1751 through to 2014 is available
from Data for the years 2014
\url{https://cdiac.ess-dive.lbl.gov/ftp/ndp030/global.1751_2014.ems}}
\format{
  This data frame contains the following columns:
  \describe{
    \item{year}{a numeric vector giving the year the measurement
was taken.}
    \item{carbon}{a numeric vector giving the total worldwide
carbon emissions from fossil fuel use, in millions of tonnes.}
  }
}
\source{
\insertRef{BodenEtAl}{DAAG}
}
\examples{
plot(fossilfuel)
}
\keyword{datasets}
