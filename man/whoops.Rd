\name{whoops}
\alias{whoops}
\docType{data}
\title{Deaths from whooping cough, in London}
\description{
  Deaths from whooping cough, in London from 1740 to 1881.  
}
\usage{data(whoops)}
\format{This is a multiple time series consisting of 3 series:
\code{wcough}, \code{ratio}, and \code{alldeaths}.
}
\source{
  Guy, W. A. 1882. Two hundred and fifty years of small pox in London.
  Journal of the Royal Statistical Society 399-443.
}
\references{
  Lancaster, H. O. 1990. Expectations of Life. Springer.
}
\examples{
data(whoops)
str(whoops)
plot(whoops) 
}
\keyword{datasets}
