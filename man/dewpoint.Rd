\name{dewpoint}
\alias{dewpoint}
\docType{data}
\title{Dewpoint Data}
\usage{dewpoint}
\description{
The \code{dewpoint} data frame has 72 rows and 3 columns.
Monthly data were obtained for a number of sites (in Australia)
and a number of months. 
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{maxtemp}{monthly minimum temperatures}
    \item{mintemp}{monthly maximum temperatures}
    \item{dewpt}{monthly average dewpoint for each combination of
minimum and maximum temperature readings (formerly dewpoint)}
  }
}
\source{
Dr Edward Linacre,
visiting fellow in the Australian National University Department
of Geography. 
}
\examples{
print("Additive Model - Example 7.5")
require(splines)
attach(dewpoint)   
ds.lm <- lm(dewpt ~ bs(maxtemp,5) + bs(mintemp,5), data=dewpoint)
ds.fit <-predict(ds.lm, type="terms", se=TRUE)
oldpar <- par(mfrow=c(1,2))
plot(maxtemp, ds.fit$fit[,1], xlab="Maximum temperature",
     ylab="Change from dewpoint mean",type="n")
lines(maxtemp,ds.fit$fit[,1])
lines(maxtemp,ds.fit$fit[,1]-2*ds.fit$se[,1],lty=2)
lines(maxtemp,ds.fit$fit[,1]+2*ds.fit$se[,1],lty=2)
plot(mintemp,ds.fit$fit[,2],xlab="Minimum temperature",
     ylab="Change from dewpoint mean",type="n")
ord<-order(mintemp)
lines(mintemp[ord],ds.fit$fit[ord,2])
lines(mintemp[ord],ds.fit$fit[ord,2]-2*ds.fit$se[ord,2],lty=2)
lines(mintemp[ord],ds.fit$fit[ord,2]+2*ds.fit$se[ord,2],lty=2)
detach(dewpoint)
par(oldpar)

}
\keyword{datasets}
