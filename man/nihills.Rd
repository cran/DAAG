\name{nihills}
\alias{nihills}
\alias{lognihills}
\docType{data}
\title{Record times for Northern Ireland mountain running events}
\description{
  Data were from the 2007 calendar for the Northern Ireland Mountain
  Running Association.
}
\usage{data(nihills)
data(lognihills)
}
\format{
  A data frame with 23 observations on the following 4 variables.
  \describe{
    \item{\code{dist}}{distances in miles}
    \item{\code{climb}}{amount of climb in feet}
    \item{\code{time}}{record time in hours for males}
    \item{\code{timef}}{record time in hours for females}
    \item{\code{logdist}}{distances, log(miles)}
    \item{\code{logclimb}}{climb, log(feet)}
    \item{\code{logtime}}{record time for males, log(hours)}
    \item{\code{logtimef}}{record time for females, log(hours)}
  }
}
\details{These data make an interesting comparison with the dataset
  \code{hills2000} in the DAAG package.}
\source{For more recent information, see
  \url{https://www.nimra.org.uk/index.php/fixtures/}
}
\examples{
data(nihills)
lm(formula = log(time) ~ log(dist) + log(climb), data = nihills)
lm(formula = log(time) ~ log(dist) + log(climb/dist), data = nihills)
lm(formula = logtime ~ logdist + I(logclimb-logdist), data = lognihills)
}
\keyword{datasets}
