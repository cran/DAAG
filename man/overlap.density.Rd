\name{overlap.density}
\alias{overlap.density}
\title{Overlapping Density Plots}
\description{
Densities for two independent samples are estimated and plotted.  
}
\usage{
overlap.density(x0, x1, frac=c(0.05, 20), plotit=TRUE)
}
\arguments{
  \item{x0}{control group measurements}
  \item{x1}{treatment group measurements}
  \item{frac}{}
  \item{plotit}{If true, a plot is produced}
}
\author{J.H. Maindonald}
\seealso{ \code{t.test} }

\examples{
data(two65)
attach(two65)
overlap.density(ambient,heated)
t.test(ambient,heated)
}
\keyword{models}
