\name{toycars}
\alias{toycars}
\title{Toy Cars Data}
\usage{toycars}
\description{
The \code{toycars} data frame has 27 rows and 3 columns.
Observations are on the
distance traveled by one of three different toy cars on
a smooth surface, starting from rest at the top of a 16 inch long ramp
tilted at varying angles.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{angle}{tilt of ramp, in degrees}
    \item{distance}{distance traveled, in meters}
    \item{car}{a numeric code (1 = first car, 2 = second car, 3 = third 
car)}
  }
}
\examples{
toycars.lm <- lm(distance ~ angle + factor(car), data=toycars)
summary(toycars.lm)
}
\keyword{datasets}
