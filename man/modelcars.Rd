\name{modelcars}
\alias{modelcars}
\non_function{}
\title{Model Car Data}
\usage{modelcars}
\description{
The \code{modelcars} data frame has 12 rows and 2 columns.
The data are for an experiment in which a model car was released
three times at each of four different distances up a 20 degree
ramp.  The experimenter recorded distances traveled from the 
bottom of the ramp across a concrete floor.  
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{distance.traveled}{a numeric vector consisting
of the lengths traveled (in cm)}
    \item{starting.point}{a numeric vector consisting
of the distance of the starting point from the top of
the ramp (in cm)}
  }
}
\source{
J.H. Maindonald
}
\examples{
plot(modelcars)
modelcars.lm <- lm(distance.traveled ~ starting.point, data=modelcars)
aov(modelcars.lm)
pause()

print("Response Curves - Example 4.6")
attach(modelcars)
stripchart(distance.traveled ~ starting.point, vertical=TRUE, pch=15,  xlab = "Distance up ramp", ylab="Distance traveled")
detach(modelcars)

}
\keyword{datasets}
