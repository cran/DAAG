\name{poissonsim}
\alias{poissonsim}
\title{Simple Poisson Regression Data Simulator}
\description{
This function simulates simple regression data from a Poisson
model.  It also has the option to create over-dispersed data
of a particular type.
}
\usage{
poissonsim(x = seq(0, 1, length=101), a = 2, b = -4, intcp.sd=NULL,
           slope.sd=NULL, seed=NULL)
}
\arguments{
  \item{x}{a numeric vector representing the explanatory variable}
  \item{a}{the regression function intercept}
  \item{b}{the regression function slope}
  \item{intcp.sd}{standard deviation of the (random) intercept}
  \item{slope.sd}{standard deviation of the (random) slope}
  \item{seed}{numeric constant}
}
\value{ a list consisting of
\item{x}{the explanatory variable vector}
\item{y}{the Poisson response vector}
}
\examples{
poissonsim()
}
\keyword{models}
