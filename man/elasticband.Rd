\name{elasticband}
\alias{elasticband}
\non_function{}
\title{Elastic Band Data}
\usage{data(elasticband)}
\description{
The \code{elasticband} data frame has 7 rows and 2 columns
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
data(elasticband)

print("Example 1.8.1")

attach(elasticband)     # R now knows where to find stretch and distance
plot(stretch, distance) # Alternative: plot(distance ~ stretch)
detach(elasticband)
pause()

print("Output of Data Frames - Example 12.3.2")

write(t(elasticband),file="bands.txt",ncol=2)

sink("bands2.txt")
elasticband   # NB: No output on screen
sink()

print("Lists - Example 12.7")

elastic.lm <- lm(distance ~ stretch, data=elasticband)
 names(elastic.lm)
 elastic.lm$coefficients
elastic.lm[["coefficients"]]
pause()

elastic.lm[[1]]
pause()

elastic.lm[1]
pause()

options(digits=3)
elastic.lm$residuals 
pause()

elastic.lm$call
pause()

 mode(elastic.lm$call)

}
\keyword{datasets}
