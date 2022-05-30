\name{simulateLinear}
\alias{simulateLinear}
\title{Simulation of Linear Models for ANOVA vs. Regression Comparison}
\description{
This function simulates a number of bivariate data sets
in which there are replicates at each level of the predictor.
The p-values for ANOVA and for the regression slope are
compared, and a lattice graphics object returned.
}
\usage{
simulateLinear(sd=2, npoints=5, nrep=4, nsets=200, graphtype="xy",
seed=21, ...)
}
\arguments{
  \item{sd}{The error standard deviation}
  \item{npoints}{Number of distinct predictor levels}
  \item{nrep}{Number of replications at each level}
  \item{nsets}{Number of simulation runs}
  \item{graphtype}{Type of graph; x-y plot (\code{graphtype="xy"}),
  overlaid density plots (\code{graphtype="density"}), or density
  plot for x-y difference (\code{graphtype="density-diff"})
  }
  \item{seed}{Random Number generator seed}
  \item{...}{Additional arguments, to be passed through to the
    lattice function that is called}
}
\value{
A lattice graphics object.
}
\author{J.H. Maindonald}
\examples{
simulateLinear()
}
\keyword{models}
