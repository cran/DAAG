\name{elastic1}
\alias{elastic1}
\alias{elastic2}
\docType{data}
\title{Elastic Band Data Replicated}
\usage{
  data(elastic1)
  data(elastic2)
  }
\description{
Both datasets
give, for each amount by which an elastic band is stretched
over the end of a ruler, the distance that the band traveled when
released.  The \code{elastic1} data frame has 7 rows.
The \code{elastic2} data frame, whose data span a wider range
of stretches and distances, has 9 rows.
}
\format{
  These data frames contains the following columns:
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
plot(elastic1)
sapply(elastic1, mean)
pause()
sapply(elastic1, function(x)mean(x))
pause()
sapply(elastic1, function(x)sum(log(x)))
pause()
yrange <- range(c(elastic1$distance, elastic2$distance))
xrange <- range(c(elastic1$stretch, elastic2$stretch))
plot(distance ~ stretch, data = elastic1, pch = 16, ylim = yrange, xlim = 
xrange)
points(distance ~ stretch, data = elastic2, pch = 15, col = 2)
legend(xrange[1], yrange[2], legend = c("Data set 1", "Data set 2"), pch = 
c(16, 15), col = c(1, 2))
elastic1.lm <- lm(distance ~ stretch, data = elastic1)
elastic2.lm <- lm(distance ~ stretch, data = elastic2)
abline(elastic1.lm)
abline(elastic2.lm, col = 2)
summary(elastic1.lm)
summary(elastic2.lm)
pause()
predict(elastic1.lm, se.fit=TRUE)
predict(elastic2.lm, se.fit=TRUE)
}
\keyword{datasets}
