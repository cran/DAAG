\name{hills2000}
\alias{hills2000}
\non_function{}
\title{Scottish Hill Races Data - 2000}
\usage{hills2000}
\description{
The record times in 2000 for 56 Scottish hill races.  We
believe the data are, for the most part, trustworthy.  
This is the subset of \code{races2000} for which \code{type}
is \code{hill}.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{dist}{distance, in miles (on the map)}
    \item{climb}{total height gained during the route, in feet}
    \item{time}{record time in hours}
    \item{timef}{record time in hours for females}
  }
}
\source{
The Scottish Running Resource, http://www.hillrunning.co.uk
}
\examples{
    pairs(hills2000)
}
\keyword{datasets}
