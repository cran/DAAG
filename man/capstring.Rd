\name{capstring}
\alias{capstring}
\title{Converts initial character of a string to upper case}
\description{This function is useful for use before
plotting, if one wants capitalized axis labels or factor
levels.
}
\usage{
capstring(names)
}
\arguments{
  \item{names}{a character vector}
}
\value{
  A character vector with upper case initial values.
}
\author{W.J. Braun}
\examples{
capstring(names(tinting)[c(3,4)])

library(lattice)
levels(tinting$agegp) <- capstring(levels(tinting$agegp))
xyplot(csoa ~ it | sex * agegp, data=tinting) 
}
\keyword{models}
