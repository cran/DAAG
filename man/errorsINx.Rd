\name{errorsINx}
\alias{errorsINx}
\title{Simulate data for straight line regression, with "errors in x".}
\description{
  Simulates $y-$ and $x-$values for the straight line regression model,
  but with $x-$values subject to random measurement error, following
  the classical \dQuote{errors in x} model. Optionally, the x-values
  can be split into two groups, with one group shifted relative to the other
}
\usage{errorsINx(mu = 12.5, n = 200, a = 15, b = 1.5, SDx=2, SDyerr = 1.5,
           timesSDx=(1:5)/2.5, gpfactor=if(missing(gpdiff))FALSE else TRUE,
           gpdiff=if(gpfactor) 1.5 else 0, layout=NULL,
           parset = simpleTheme(alpha = 0.75, col = c("black","gray45"),
             col.line = c("black","gray45"), lwd=c(1,1.5), pch=c(1,2),
           lty=c(1,2)), print.summary=TRUE, plotit=TRUE, xrelation="same")
}
\arguments{
  \item{mu}{Mean of $z$}
  \item{n}{Number of points}
  \item{a}{Intercept in model where $z$ is measured without error}
  \item{b}{Slope in model where $z$ is measured without error}
  \item{SDx}{SD of $z$-values, measured without error}
  \item{SDyerr}{SD of error term in \code{y} where $z$ is measured without error}
  \item{timesSDx}{SD of measurement error is \code{timesSDx}, as a
    multiple of \code{SDx}}
  \item{gpfactor}{Should x-values be split into two groups, with one
  shifted relative to the other?}
  \item{gpdiff}{Amount of shift of one group of z-values relative to the other}
  \item{layout}{Layout for lattice graph, if requested}
  \item{parset}{Parameters to be supplied to the lattice plot, if any}
  \item{print.summary}{Print summary information on fits?}
  \item{plotit}{logical: plot the data?}
  \item{xrelation}{character: sets the x-axis \code{relation} component of
    \code{scales} to \code{"same"} or \code{"free"} or (though this does
    not make make sense here) \code{"sliced"}.}
}
\details{
  The argument \code{timesSDx} can be a numeric vector.
  One set of $x$-values that are contaminated with measurement error
  is simulated for each element of \code{timesSDx}.
}
\value{
  \item{gph}{the trellis graphics object}
  \item{mat}{
  A matrix, with \code{length(timesSDx)+2} columns.  Values of $z$ are
  in the first column. There is one further column (x with error) for
  each element of \code{timesSDx}, followed by a column for $y$.
  If there is a grouping variable, a further column identifies the
  groups.}
}
\references{Data Analysis and Graphics Using R, 3rd edn, Section 6.7}
\author{John Maindonald}
\examples{
library(lattice)
errorsINx()
errorsINx(gpdiff=2, timesSDx=1.25, SDyerr=2.5, n=80)
}
\keyword{datagen}
