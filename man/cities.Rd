\name{cities}
\alias{cities}
\non_function{}
\title{Populations of Major Canadian Cities (1992-96)}
\usage{cities}
\description{
Population estimates for several Canadian cities.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{CITY}{a factor, consisting of the city names}
    \item{REGION}{a factor with 5 levels (ATL=Atlantic, ON=Ontario,
QC=Quebec, PR=Prairies, WEST=Alberta and British Columbia) representing the location
of the cities}
    \item{POP1992}{a numeric vector giving population in 1000's for 1992}
    \item{POP1993}{a numeric vector giving population in 1000's for 1993}
    \item{POP1994}{a numeric vector giving population in 1000's for 1994}
    \item{POP1995}{a numeric vector giving population in 1000's for 1995}
    \item{POP1996}{a numeric vector giving population in 1000's for 1996}
  }
}
\source{
Statistics Canada
}
\examples{
cities$have <- factor((cities$REGION=="ON")|(cities$REGION=="WEST"))
plot(POP1996~POP1992, data=cities, col=as.integer(cities$have))
}
\keyword{datasets}
