\name{simulatelinear}
\alias{simulatelinear}
\title{Simulation of Linear Models for ANOVA vs. Regression Comparison}
\description{
This function simulates a number of bivariate data sets
in which there are replicates at each level of the predictor.
The p-values for ANOVA and for the regression slope are 
compared.
}
\usage{
simulatelinear(sd=2, npoints=5, nrep=4, nsets=200, type="xy", seed=21)
}
\arguments{
  \item{sd}{The error standard deviation}
  \item{npoints}{Number of distinct predictor levels}
  \item{nrep}{Number of replications at each level}
  \item{nsets}{Number of simulation runs}
  \item{type}{Type of data}
  \item{seed}{Random Number generator seed}
}
\value{
The proportion of regression p-values that are less than the ANOVA p-values is
printed
}
\author{J.H. Maindonald}
\examples{
simulatelinear()
}
\keyword{models}
