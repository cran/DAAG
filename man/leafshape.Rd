\name{leafshape}
\alias{leafshape}
\title{Full Leaf Shape Data Set}
\usage{leafshape}
\description{
Leaf length, width and petiole measurements taken at various
sites in Australia.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{bladelen}{leaf length (in mm)}
    \item{petiole}{a numeric vector}
    \item{bladewid}{leaf width (in mm)}
    \item{latitude}{latitude}
    \item{logwid}{natural logarithm of width}
    \item{logpet}{logarithm of petiole}
    \item{loglen}{logarithm of length}
    \item{arch}{leaf architecture (0 = plagiotropic, 1 = orthotropic}
    \item{location}{a factor with levels}
\code{Sabah}, \code{Panama}, \code{Costa Rica},
\code{N Queensland}, \code{S Queensland}, 
\code{Tasmania} }
  }
}
\source{
  King, D.A. and Maindonald, J.H. 1999. Tree architecture in relation to
leaf dimensions and tree stature in temperate and tropical rain
forests. Journal of Ecology 87: 1012-1024.
}
\keyword{datasets}
