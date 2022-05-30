\name{orings}
\alias{orings}
\docType{data}
\title{Challenger O-rings Data}
\usage{orings}
\description{
Record of the number and type of O-ring failures prior to the
tragic Challenger mission in January, 1986.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Temperature}{O-ring temperature for each test firing or
actual launch of the shuttle rocket engine}
    \item{Erosion}{Number of erosion incidents }
    \item{Blowby}{Number of blowby incidents}
    \item{Total}{Total number of incidents}
  }
}
\source{
 Presidential Commission on the Space Shuttle Challenger Accident, 
Vol. 1, 1986: 129-131.  
}
\references{
 Tufte, E. R. 1997. Visual Explanations. Graphics Press, Cheshire,
Connecticut, U.S.A.
}
\examples{
oldpar <- par(mfrow=c(1,2))
plot(Total~Temperature, data = orings[c(1,2,4,11,13,18),]) # the 
               # observations included in the pre-launch charts
plot(Total~Temperature, data = orings)
par(oldpar)
}
\keyword{datasets}
