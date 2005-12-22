\name{poxetc}
\alias{poxetc}
\docType{data}
\title{Deaths from various causes, in London from 1629 till 1881, with gaps}
\description{
  Deaths from "flux" or smallpox, measles, all causes, and ratios
  of the the first two categories to total deaths.
}
\usage{data(poxetc)}
\format{This is a multiple time series consisting of 5 series:
\code{fpox}, \code{measles}, \code{all}, \code{fpox2all}, \code{measles2all}.
}
\details{
}
\source{
  Guy, W. A. 1882. Two hundred and fifty years of small pox in London.
  Journal of the Royal Statistical Society 399-443.
}
\references{
  Lancaster, H. O. 1990. Expectations of Life. Springer.
}
\examples{
data(poxetc)
str(poxetc)
plot(poxetc) 
}
\keyword{datasets}
