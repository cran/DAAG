\name{bounce}
\alias{bounce}
\title{Separate plotting positions for labels, to avoid overlap}
\description{  
  Return univariate plotting positions in which neighboring points are
  separated, if and as necessary, so that they are the specified minimum
  distance apart.  
}
\usage{
bounce(y, d, log = FALSE)
}
\arguments{
  \item{y}{A numeric vector of plotting positions}
  \item{d}{Minimum required distance between neighboring positions}
  \item{log}{\code{TRUE} if values are will be plotted on a logarithmic scale.}
}
\details{  
  The centroid(s) of groups of points that are moved relative to each
  other remain the same.
}
\value{  
  A vector of values such that, when plotted along a line, neighboring
  points are the required minimum distance apart.  
}
\author{John Maindonald}
\note{
  If values are plotted on a logarithmic scale, \code{d} is the required
  distance apart on that scale. If a base other than 10 is required, set
  \code{log} equal to that base.  (Note that base 10 is the default for
  \code{plot} with \code{log=TRUE}.)
}
\seealso{See also \code{\link{onewayPlot}}}
\examples{
bounce(c(4, 1.8, 2, 6), d=.4)
bounce(c(4, 1.8, 2, 6), d=.1, log=TRUE)
}
\keyword{utilities}
