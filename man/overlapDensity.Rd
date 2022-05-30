\name{overlapDensity}
\alias{overlapDensity}
\alias{overlap.density}
\title{Overlapping Density Plots}
\description{
Densities for two distinct samples are estimated and plotted.
}
\usage{
overlapDensity(x0, x1, ratio = c(0.05, 20), ratio.number = FALSE,
          plotvalues = c("Density", "Numbers"), gpnames = c("Control", "Treatment"),
          cutoffs = c(lower = TRUE, upper = TRUE), bw = FALSE,
          xlab = "Score", ylab = NULL,
          col = 1:2, lty = 1:2, lwd = c(1, 1), ...)

overlap.density(x0, x1, ratio = c(0.05, 20), ratio.number = FALSE,
          plotvalues = c("Density", "Numbers"), gpnames = c("Control", "Treatment"),
          cutoffs = c(lower = TRUE, upper = TRUE), bw = FALSE,
          xlab = "Score", ylab = NULL,
          col = 1:2, lty = 1:2, lwd = c(1, 1), ...)
}
\arguments{
  \item{x0}{control group measurements}
  \item{x1}{treatment group measurements}
  \item{ratio}{if not \code{NULL}, the range within which the relative
           number per unit interval (\code{ratio.number=TRUE}) or
           relative probability density (\code{ratio.number=FALSE})
           of observations from the two groups are required to lie
	   will be used to determine lower and upper bounds on the
	   values of \code{x0} and \code{x1}.
	   [The relative numbers at any point are estimated from
	   (density1*n1)/(density0*x0)]}
	 \item{ratio.number}{If TRUE (default), then \code{ratio}
	   is taken as the ratio of number of points per unit
	   interval}
	 \item{plotvalues}{If set to \code{Number} then the y-axis
	   scale is chosen so that total area undere the curve is equal
	   to the sample size; otherwise (\code{plotvalues="Density"})
	   total area under each cueve is 1. Any other setting does
	 not give a plot.}
  \item{gpnames}{Names of the two samples}
  \item{cutoffs}{logical vector, indicating whether density estimates
should be truncated below (lower=TRUE) or above (upper=TRUE)}
  \item{bw}{logical, indicates whether to overwrite with a gray
scale plot}
  \item{xlab}{Label for x-axis}
  \item{ylab}{Label for y-axis}
  \item{col}{standard color parameter}
  \item{lty}{standard line type preference}
  \item{lwd}{standard line width preference}
  \item{...}{Other parameters to be passed to \code{plot()}}
}
\author{J.H. Maindonald}
\seealso{ \code{t.test} }

\examples{
attach(two65)
overlapDensity(ambient,heated)
t.test(ambient,heated)
}
\keyword{models}
