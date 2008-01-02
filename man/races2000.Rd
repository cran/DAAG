\name{races2000}
\alias{races2000}
\title{Scottish Hill Races Data - 2000}
\usage{races2000}
\description{
The record times in 2000 for 77 Scottish long distance
races.  We
believe the data are, for the most part, trustworthy.  However,
the \code{dist} variable for Caerketton (record 58) seems
to have been variously recorded as 1.5 mi and 2.5 mi.  
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{dist}{distance, in miles (on the map)}
    \item{climb}{total height gained during the route, in feet}
    \item{time}{record time in hours}
    \item{timef}{record time in hours for females}
    \item{type}{a factor, with levels indicating type of race, 
i.e. hill, marathon, relay, uphill or other}
  }
}
\source{
The Scottish Running Resource, http://www.hillrunning.co.uk
}
\examples{
    pairs(races2000[,-5])
}
\keyword{datasets}
