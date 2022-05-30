\name{mignonette}
\alias{mignonette}
\docType{data}
\title{Darwin's Wild Mignonette Data}
\usage{mignonette}
\description{
Data compare the heights of crossed plants with
self-fertilized plants of the wild mignonette
\emph{reseda lutea}.  Plants were paired within the
pots in which they were grown, with one on one side and
one on the other.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{cross}{heights of 
the crossed plants}
    \item{self}{heights of
the self-fertilized plants}
  }
}
\source{
Darwin, Charles. 1877.  The Effects of Cross and Self
Fertilisation in the Vegetable Kingdom.  Appleton and Company, New
York, page 118.
}
\examples{
print("Is Pairing Helpful? - Example 4.3.1")

attach(mignonette)
plot(cross ~ self, pch=rep(c(4,1), c(3,12))); abline(0,1) 
abline(mean(cross-self), 1, lty=2)
detach(mignonette)
}
\keyword{datasets}
