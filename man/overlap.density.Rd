\name{overlap.density}
\alias{overlap.density}
\title{Overlapping Density Plots}
\description{
Densities for two independent samples are estimated and plotted.  
}
\usage{
overlap.density(x0, x1, ratio=c(0.05, 20), compare.numbers=TRUE, 
plotit=TRUE, gpnames=c("Control", "Treatment"), xlab="Score")
}
\arguments{
  \item{x0}{control group measurements}
  \item{x1}{treatment group measurements}
  \item{ratio}{the range within which the relative numbers of
           observations from the two groups are
           required to lie.  [The relative numbers at
           any point are estimated from (density1*n1)/(density0*x0)]}
  \item{compare.numbers}{If TRUE (default), then density plots
are scaled to have total area equal to the sample size; otherwise
total area under each density is 1}
  \item{plotit}{If TRUE, a plot is produced}
  \item{gpnames}{Names of the two samples}
  \item{xlab}{Label for x-axis}
}
\author{J.H. Maindonald}
\seealso{ \code{t.test} }

\examples{
attach(two65)
overlap.density(ambient,heated)
t.test(ambient,heated)
}
\keyword{models}
