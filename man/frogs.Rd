\name{frogs}
\alias{frogs}
\docType{data}
\title{Frogs Data}
\usage{frogs}
\description{
The \code{frogs} data frame has 212 rows and 11 columns.
The data are on the distribution of the Southern Corroboree
frog, which occurs in the Snowy Mountains area of New South Wales,
Australia.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{pres.abs}{0 = frogs were absent, 1 = frogs were present}
    \item{northing}{reference point}
    \item{easting}{reference point}
    \item{altitude}{altitude , in meters}
    \item{distance}{distance in meters to nearest extant population}
    \item{NoOfPools}{number of potential breeding pools}
    \item{NoOfSites}{(number of potential breeding sites within a 2 km 
radius}
    \item{avrain}{mean rainfall for Spring period}
    \item{meanmin}{mean minimum Spring temperature}
    \item{meanmax}{mean maximum Spring temperature}
  }
}
\source{
 Hunter, D. (2000) The conservation and demography of 
the southern corroboree frog (Pseudophryne corroboree). M.Sc. thesis,
University of Canberra, Canberra.
}
\examples{
print("Multiple Logistic Regression - Example 8.2")

plot(northing ~ easting, data=frogs, pch=c(1,16)[frogs$pres.abs+1],
  xlab="Meters east of reference point", ylab="Meters north")
pairs(frogs[,4:10])
attach(frogs)
pairs(cbind(altitude,log(distance),log(NoOfPools),NoOfSites),
  panel=panel.smooth, labels=c("altitude","log(distance)",
  "log(NoOfPools)","NoOfSites"))
detach(frogs)

frogs.glm0 <- glm(formula = pres.abs ~ altitude + log(distance) +
  log(NoOfPools) + NoOfSites + avrain + meanmin + meanmax,
  family = binomial, data = frogs)
summary(frogs.glm0)

frogs.glm <- glm(formula = pres.abs ~ log(distance) + log(NoOfPools) + 
meanmin +
  meanmax, family = binomial, data = frogs)
oldpar <- par(mfrow=c(2,2))
termplot(frogs.glm, data=frogs)

termplot(frogs.glm, data=frogs, partial.resid=TRUE)

cv.binary(frogs.glm0)   # All explanatory variables
pause()

cv.binary(frogs.glm)    # Reduced set of explanatory variables

for (j in 1:4){
 rand <- sample(1:10, 212, replace=TRUE)
 all.acc <- cv.binary(frogs.glm0, rand=rand, print.details=FALSE)$acc.cv
 reduced.acc <- cv.binary(frogs.glm, rand=rand, print.details=FALSE)$acc.cv
 cat("\nAll:", round(all.acc,3), "  Reduced:", round(reduced.acc,3))
}

}
\keyword{datasets}
