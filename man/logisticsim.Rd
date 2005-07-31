\name{logisticsim}
\alias{logisticsim}
\title{Simple Logistic Regression Data Simulator}
\description{
This function simulates simple regression data from a 
logistic model.
}
\usage{
logisticsim(x = seq(0, 1, length=101), a = 2, b = -4, seed=NULL)
}
\arguments{
  \item{x}{a numeric vector representing the explanatory variable}
  \item{a}{the regression function intercept}
  \item{b}{the regression function slope}
  \item{seed}{numeric constant}
}
\value{ a list consisting of
\item{x}{the explanatory variable vector}
\item{y}{the Poisson response vector}
}
\examples{
logisticsim()
}
\keyword{models}
