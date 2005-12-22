\name{antigua}
\alias{antigua}
\title{Averages by block of yields for the Antigua Corn data}
\description{
  These data frames have yield averages by blocks (parcels). The
\code{ant111b} data set is a subset of this.
}
\usage{antigua}
\format{ A data frame with 324 observations on 7 variables.
  \describe{
    \item{id}{a numeric vector}
    \item{site}{a factor with 8 levels.}
    \item{block}{a factor with levels \code{I} \code{II} \code{III} \code{IV}}
    \item{plot}{a numeric vector}
    \item{trt}{a factor consisting of 12 levels}
    \item{ears}{a numeric vector; note that -9999 is used as a missing value code.}
    \item{harvwt}{a numeric vector; the average yield}
  }
}
\source{
  Andrews DF; Herzberg AM, 1985. Data. A Collection of Problems from
  Many Fields for the Student and Research Worker. Springer-Verlag.
  (pp. 339-353)
}
\keyword{datasets}
