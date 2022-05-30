\name{sumry}
\alias{sumry}
\title{
A more flexible alternatives to \code{summary}.
}
\description{
At present this has a method only for \code{glm} objects.
The function \code{print.sumry.glm} allows greater control over 
what is printed.
}
\usage{
sumry(object, ...)
}
\arguments{
  \item{object}{
An object for with a summary is required.  At present, this must be a glm object.
}
  \item{\dots}{
additional arguments affecting the summary produced.
}
}
\value{
Returns summary information.
}
\author{
John Maindonald
}
\seealso{
\code{\link{print.sumry.glm}}, \code{\link{sumry}}, \code{\link{glm}}
}
\keyword{models}
\keyword{regression}
