\name{plotSimDiags}
\alias{plotSimDiags}
\alias{plotSimScat}
\title{
  Show regression diagnostics or a scatterplot for simulated data.
}
\description{
  Generic function for returning graphics objects that show regression
  diagnostics or (for \code{plotSimScat}) scatterplot(s) for simulated
  data. Currently the only plot method for \code{plotSimDiags} is
  \code{\link{plotSimDiags.lm}} that accepts \code{lm} objects and
  objects of a class that inherits from \code{lm}
}
\usage{
plotSimDiags(obj, ...)
plotSimScat(obj, sigma = NULL, layout = c(4, 1), type = c("p",
    "r"), show = c("points", "residuals"), ...)
}
\arguments{
  \item{obj}{
    An object of class code{lm}, or of a class that inherits from \code{lm}.
    For \code{plotSimScat}, a straight line \code{lm} object.
}
\item{sigma}{Standard error of residual (\code{plotSimScat}).}
\item{layout}{
    Controls the number of simulations and the layout of the plots.
    For example \code{layout=c(3,4)} will give 12 plots in a 3 by 4 layout.
  }
  \item{type}{'"p"' denotes points, '"r"' denotes a regression line,
    '"smooth"' adds a smooth curve  (\code{plotSimScat}).}
  \item{show}{'"points"' shows the y-variable values, '"residuals"'
    shows residuals on the vertical axis (\code{plotSimScat}).}
  \item{\dots}{
    For other parameters that can be passed to the lattice function call(s),
    see \code{\link{plotSimDiags.lm}}
}
}
\value{
  A list of lattice (trellis) graphics objects. Or if \code{which} is of
  length 1, a lattice graphics object. See the help page for
  \code{\link{plotSimDiags.lm}}
  For \code{plotSimScat}, a lattice graphics object.
}
\references{
See the help page for \code{\link{plot.lm}}
}
\author{
John Maindonald
}
\seealso{
\code{\link{plot.lm}}, \code{\link{plotSimDiags.lm}}
}
\examples{
htwt.lm <- lm(height ~ weight, data=women)
gphlist <- plotSimDiags(obj=htwt.lm, which=c(1:3,5))
}
\keyword{regression}
\keyword{graphics}
