\name{overlapDensity}
\alias{overlapDensity}
\title{Overlapping Density Plots}
\description{
Densities for two independent samples are estimated and plotted.  
}
\usage{
overlapDensity(x0, x1, ratio = c(0.05, 20), compare.numbers = FALSE,
            plotit = TRUE, gpnames = c("Control", "Treatment"),
            cutoffs=c(lower=TRUE, upper=TRUE), bw=FALSE,
            xlab = "Score", col=1:2, lty=1:2)
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
  \item{cutoffs}{logical vector, indicating whether density estimates
should be truncated below (lower=TRUE) or above (upper=TRUE)}
  \item{bw}{logical, indicates whether to overwrite with a gray
scale plot}
  \item{xlab}{Label for x-axis}
  \item{col}{standard color parameter}
  \item{lty}{standard line type preference}
}
\author{J.H. Maindonald}
\seealso{ \code{t.test} }

\examples{
attach(two65)
overlapDensity(ambient,heated)
t.test(ambient,heated)
}
\keyword{models}
