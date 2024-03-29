\name{allbacks}
\alias{allbacks}
\docType{data}
\title{Measurements on a Selection of Books}
\usage{allbacks}
\description{
The \code{allbacks} data frame gives measurements
on the volume and weight of 15 books, some of which
are softback (pb) and some of which are hardback (hb).  Area
of the hardback covers is also included.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{volume}{book volumes
in cubic centimeters}
    \item{area}{hard board cover
areas in square centimeters}
    \item{weight}{book weights in grams}
    \item{cover}{a factor with levels
   \code{hb} hardback,   \code{pb} paperback}
  }
}
\source{
The bookshelf of J. H. Maindonald.
}
\examples{
print("Multiple Regression - Example 6.1")
attach(allbacks)
volume.split <- split(volume, cover)
weight.split <- split(weight, cover)
plot(weight.split$hb ~ volume.split$hb, pch=16, xlim=range(volume), ylim=range(weight),
     ylab="Weight (g)", xlab="Volume (cc)")
points(weight.split$pb ~ volume.split$pb, pch=16, col=2)
pause()

allbacks.lm <- lm(weight ~ volume+area)
summary(allbacks.lm)
detach(allbacks)
pause()

anova(allbacks.lm)
pause()

model.matrix(allbacks.lm)
pause()

print("Example 6.1.1")
allbacks.lm0 <- lm(weight ~ -1+volume+area, data=allbacks)
summary(allbacks.lm0)
pause()

print("Example 6.1.2")
oldpar <- par(mfrow=c(2,2))
plot(allbacks.lm0)
par(oldpar)
allbacks.lm13 <- lm(weight ~ -1+volume+area, data=allbacks[-13,])
summary(allbacks.lm13)
pause()

print("Example 6.1.3")
round(coef(allbacks.lm0),2)  # Baseline for changes
round(lm.influence(allbacks.lm0)$coef,2)
}
\keyword{datasets}
