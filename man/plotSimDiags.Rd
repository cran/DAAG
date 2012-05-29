\name{plotSimDiags}
\alias{plotSimDiags}
\title{
  Return lattice graphics objects that show regression diagnostics
  for simulated data.
}
\description{
  Generic function for returning graphics objects that show regression
  diagnostics for simulated data. Currently the only plot method is
  \code{\link{plotSimDiags.lm}} that accepts \code{lm} objects and
  objects of a class that inherits from \code{lm}
}
\usage{
plotSimDiags(obj, ...)
}
\arguments{
  \item{obj}{
An object of class code{lm}, or of a class that inherits from \code{lm}
}
  \item{\dots}{
    Other parameters that can be passed to the lattice function call(s).
    See \code{\link{plotSimDiags.lm}}
}
}
\value{
  A list of lattice graphics objects. Or if \code{which} is of length 1,
  a lattice graphics object. See the help page for
  \code{\link{plotSimDiags.lm}}
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
htwt.lm <- lm(height ~ weight, data=women)
gphlist <- plotSimDiags(obj=htwt.lm, which=c(1:3,5))
## The function is currently defined as
function (obj, ...)
{
    UseMethod("plotSimDiags")
  }
}
\keyword{regression}
\keyword{graphics}
