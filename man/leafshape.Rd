\name{leafshape}
\alias{leafshape}
\alias{leafshape17}
\docType{data}
\title{Full Leaf Shape Data Set}
\usage{
data(leafshape)
data(leafshape17)
}
\description{
Leaf length, width and petiole measurements taken at various
sites worldwide.  The \code{leafshape17} data frame is the
subset that has data for North Queensland sites.
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
    \item{location}{a factor with levels
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
\examples{
library(MASS)
leaf17.lda <- lda(arch ~ logwid+loglen, data=leafshape17)
leaf17.hat <- predict(leaf17.lda)
leaf17.lda
 table(leafshape17$arch, leaf17.hat$class)
pause()

tab <- table(leafshape17$arch, leaf17.hat$class)
 sum(tab[row(tab)==col(tab)])/sum(tab)
leaf17cv.lda <- lda(arch ~ logwid+loglen, data=leafshape17, CV=TRUE)
tab <- table(leafshape17$arch, leaf17cv.lda$class)
pause()

leaf17.glm <- glm(arch ~ logwid + loglen, family=binomial, data=leafshape17)
 options(digits=3)
summary(leaf17.glm)$coef
pause()

leaf17.one <- cv.binary(leaf17.glm)
table(leafshape17$arch, round(leaf17.one$internal))     # Resubstitution
pause()

table(leafshape17$arch, round(leaf17.one$cv))           # Cross-validation
}
\keyword{datasets}
