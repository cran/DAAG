\name{plotSimDiags}
\alias{plotSimDiags}
\title{
Diagnostic plots for simulated data
}
\description{
  This provides diagnostic plots, closely equivalent to those provided by
  \code{\link{plot.lm}}, for simulated data.  By default, simulated data
  are for the fitted model.  Alternatively, simulated data can be
  supplied, making it possible to check the effct of fitting, e.g.,
  an AR1 model.
}
\usage{
plotSimDiags(obj, simvalues = NULL, seed = NULL,
types = NULL, which = c(1:3, 5), layout = c(4, 1), qqline=TRUE,
cook.levels = c(0.5, 1), caption = list("Residuals vs Fitted",
"Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage",
expression("Cook's dist vs Leverage  " * h[ii]/(1 - h[ii]))),
...)
}
\arguments{
  \item{obj}{
Fitted model object - \code{lm} or an object inheriting from \code{lm}
}
  \item{simvalues}{
Optional matrix of simulated data.
}
  \item{seed}{
Random number seed - set this to make results repeatable.
}
\item{types}{
  If set, this should be a list with six elements, ordinarily with
  each list element either \code{"p"} or \code{c("p","smooth")} or
  (\code{which=2}, \code{which=6}) NULL or (\code{which=4}) "h"
}
  \item{which}{
Set to be a subset of the numbers 1 to 6, as for \code{\link{plot.lm}}
}
  \item{layout}{
    Controls the number of simulations and the layout of the plots.
    For example \code{layout=c(3,4)} will give 12 plots in a 3 by 4 layout.
  }
  \item{qqline}{logical: add line to normal Q-Q plot}
  \item{cook.levels}{
Levels of Cook's statistics for which contours are to be plotted.
}
  \item{caption}{list: Captions for the six graphs
  }
  \item{...}{Other parameters to be passed to plotting functions}
}
\details{
  Diagnotic plots from repeated simulations from the fitted model
  provide a useful indication of the range of variation in the model
  diagnistics that are consistent with the fitted model.
}
\value{
  A list of lattice graphics objects is returned, one for each value of
  \code{which}.  List elements for which a graphics object is not
  returned are set to NULL.  Or if \code{which} is of length 1,
  a lattice graphics object.
\item{residVSfitted}{Residuals vs fitted}
\item{normalQQ}{Normal quantile-quantile plot}
\item{scaleVSloc}{Scale versus location}
\item{CookDist}{Cook's distance vs observation number}
\item{residVSlev}{Standardized residuals (for GLMs, standardized
  Pearson residuals) vs leverage}
\item{CookVSlev}{Cook's distance vs leverage}
For the default \code{which=c(1:3,5)}, list items 1, 2, 3 and 5 above
contain graphics objects, with list elements 4 and 6 set to \code{NULL}.
}
\references{
See \code{\link{plot.lm}}
}
\author{
John Maindonald, with some code chunks adapted from \code{plot.lm}
}
\note{
  The graphics objects contained in individual list elements can be
  extracted for printing, or updating and printing, as required.
  If the value is returned to the command line, list elements that
  are not \code{NULL} will be printed in turn.
}
\seealso{
\code{\link{plot.lm}}, \code{\link{lmdiags}}
}
\examples{
women.lm <- lm(height ~ weight, data=women)
gphlist <- plotSimDiags(obj=women.lm, which=c(1:3,5))
}
\keyword{regression}
\keyword{graphics}
