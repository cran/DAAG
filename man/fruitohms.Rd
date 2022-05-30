\name{fruitohms}
\alias{fruitohms}
\docType{data}
\title{Electrical Resistance of Kiwi Fruit}
\usage{fruitohms}
\description{
Data are from a study that examined how the electrical
resistance of a slab of kiwifruit changed with the apparent
juice content.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{juice}{apparent juice content (percent) }
    \item{ohms}{electrical resistance (in ohms)}
  }
}
\source{
Harker, F. R. and Maindonald J.H. 1994. Ripening of nectarine
fruit. \emph{Plant Physiology} 106: 165 - 171.
}
\examples{
plot(ohms ~ juice, xlab="Apparent juice content (\%)",ylab="Resistance (ohms)", data=fruitohms)
lines(lowess(fruitohms$juice, fruitohms$ohms), lwd=2)
pause()

require(splines)
attach(fruitohms)
plot(ohms ~ juice, cex=0.8, xlab="Apparent juice content (\%)",
     ylab="Resistance (ohms)", type="n")
fruit.lmb4 <- lm(ohms ~ bs(juice,4))
ord <- order(juice)
lines(juice[ord], fitted(fruit.lmb4)[ord], lwd=2)
ci <- predict(fruit.lmb4, interval="confidence")
lines(juice[ord], ci[ord,"lwr"])
lines(juice[ord], ci[ord,"upr"])
}
\keyword{datasets}
