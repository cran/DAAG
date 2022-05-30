\name{droughts}
\alias{droughts}
\docType{data}
\title{Periods Between Rain Events}
\usage{droughts}
\description{
Data collected at Winnipeg International Airport (Canada)
on periods (in days) between rain events.  
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{length}{the length of time from the completion of
the last rain event to the beginning of the next rain event.}
    \item{year}{the calendar year.}
  }
}
\examples{
  boxplot(length ~ year, data=droughts)
  boxplot(log(length) ~ year, data=droughts)
  hist(droughts$length, main="Winnipeg Droughts", xlab="length (in days)")
  hist(log(droughts$length), main="Winnipeg Droughts", xlab="length (in days, log scale)")

}
\keyword{datasets}
