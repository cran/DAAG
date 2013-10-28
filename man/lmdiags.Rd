\name{lmdiags}
\alias{lmdiags}
\title{
Return data required for diagnostic plots
}
\description{
  This extracts the code that provides the major part of the
  statistical information used by plot.lm, leaving out the
  code used to provide the graphs
}
\usage{
lmdiags(x, which = c(1L:3L, 5L), cook.levels = c(0.5, 1), hii=NULL)
}
\arguments{
  \item{x}{
    This must be an  object of class \code{lm} object, or that inherits from
    an object of class  \code{lm}.
}
\item{which}{a subset of the numbers '1:6', indicating the plots for
  which statistical information is required
}
  \item{cook.levels}{
Levels for contours of \code{cook.levels}, by default \code{c(0.5,1)}
}
\item{hii}{Diagonal elements for the hat matrix.  If not supplied
(\code{hii=NULL}), they will be calculated from the argument \code{x}.}
}
\details{
See \code{\link{plot.lm}} for additional information.
}
\value{
  \item{yh}{fitted values}
  \item{rs}{standardized residuals (for \code{glm} models standardized
  deviance residuals)}
  \item{yhn0}{As \code{yh}, but omitting observations with zero weight}
  \item{cook}{Cook's statistics}
  \item{rsp}{standardized residuals (for \code{glm} models standardized
  Pearson residuals)}
}
\references{
See references for \code{\link{plot.lm}}
}
\author{
John Maindonald, using code that John Maindonald, Martin Maechler and
others had contributed to \code{\link{plot.lm}}
}
\note{
  This function is designed, in the first place, for use in connection
  with \code{\link{plotSimDiags}}, used to give simulations of
  diagnostic plots for \code{lm} objects.
}
\seealso{
\code{\link{plotSimDiags}}, \code{\link{plot.lm}}
}
\examples{
women.lm <- lm(weight ~ height, data=women)
veclist <- lmdiags(x=women.lm)
## Returns the statistics that are required for graphs 1, 2, 3, and 5
}
\keyword{regression}
