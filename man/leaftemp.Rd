\name{leaftemp}
\alias{leaftemp}
\non_function{}
\title{Leaf and Air Temperature Data}
\usage{data(leaftemp)}
\description{
These data consist of measurements of vapour pressure and of the 
difference between leaf and air temperature.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{CO2level}{Carbon Dioxide level
\code{low}, \code{medium}, \code{high} }
    \item{vapPress}{Vapour pressure}
    \item{tempDiff}{Difference between leaf and air temperature}
    \item{BtempDiff}{a numeric vector}
  }
}
\source{
Katharina Siebke and Susan von Cammerer, Australian National University.
}
\examples{
data(leaftemp)
print("Fitting Multiple Lines - Example 7.3")

leaf.lm1 <- lm(tempDiff ~ 1 , data = leaftemp)
leaf.lm2 <- lm(tempDiff ~ vapPress, data = leaftemp)
leaf.lm3 <- lm(tempDiff ~ CO2level + vapPress, data = leaftemp)
leaf.lm4 <- lm(tempDiff ~ CO2level + vapPress + vapPress:CO2level,
  data = leaftemp)

anova(leaf.lm1, leaf.lm2, leaf.lm3, leaf.lm4)

summary(leaf.lm2)
plot(leaf.lm2)

}
\keyword{datasets}
