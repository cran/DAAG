\name{component.residual}
\alias{component.residual}
\title{Component + Residual Plot}
\description{
Component + Residual plot for a term in a \code{lm} model.
}
\usage{
component.residual(lm.obj, which = 1, xlab = "Component",
    ylab = "C+R")
}
\arguments{
  \item{lm.obj}{ A \code{lm} object }
  \item{which}{numeric code for the term in the \code{lm} formula to be 
plotted}
  \item{xlab}{label for the x-axis}
  \item{ylab}{label for the y-axis}
}
\value{
A scatterplot with a smooth curve overlaid.
}
\author{J.H. Maindonald}
\seealso{\code{\link{lm}}}

\examples{
mice12.lm <- lm(brainwt ~ bodywt + lsize, data=litters)
oldpar <- par(mfrow = c(1,2))
component.residual(mice12.lm, 1, xlab = "Body weight", ylab= "t(Body weight) + e")
component.residual(mice12.lm, 2, xlab = "Litter size", ylab= "t(Litter size) + e")
par(oldpar)
}
\keyword{models}
