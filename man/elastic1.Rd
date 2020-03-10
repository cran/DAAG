\name{elastic1}
\alias{elastic1}
\title{Elastic Band Data Replicated}
\usage{elastic1}
\description{
The \code{elastic1} data frame has 7 rows and 2 columns
giving, for each amount by which an elastic band is stretched
over the end of a ruler, the distance that the band traveled when
released.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{stretch}{the amount by which the
elastic band was stretched}
    \item{distance}{the distance traveled}
  }
}
\source{
 J. H. Maindonald
}
\examples{
\dontrun{
plot(elastic1)

print("Inline Functions - Example 12.2.2")
sapply(elastic1, mean)
pause()

sapply(elastic1, function(x)mean(x))
pause()

sapply(elastic1, function(x)sum(log(x)))
pause()

print("Data Output - Example 12.3.2")
write.table(elastic1, file="bandsframe.txt")
}
}
\keyword{datasets}
