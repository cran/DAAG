\name{cfseal}
\alias{cfseal}
\docType{data}
\title{Cape Fur Seal Data}
\usage{cfseal}
\description{
The \code{cfseal} data frame has 30 rows and 11 columns consisting
of weight measurements for various organs taken from 30 Cape Fur
Seals that died as an unintended consequence of commercial fishing.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{age}{a numeric vector}
    \item{weight}{a numeric vector}
    \item{heart}{a numeric vector}
    \item{lung}{a numeric vector}
    \item{liver}{a numeric vector}
    \item{spleen}{a numeric vector}
    \item{stomach}{a numeric vector}
    \item{leftkid}{a numeric vector}
    \item{rightkid}{a numeric vector}
    \item{kidney}{a numeric vector}
    \item{intestines}{a numeric vector}
  }
}
\source{
 Stewardson, C.L., Hemsley, S., Meyer, M.A., Canfield,
P.J. and Maindonald, J.H. 1999.  Gross and microscopic visceral
anatomy of the male Cape fur seal, Arctocephalus pusillus pusillus
(Pinnepedia: Otariidae), with reference to organ size and growth.
Journal of Anatomy (Cambridge) 195: 235-255.  (WWF project ZA-348)
}
\examples{
print("Allometric Growth - Example 5.7")

cfseal.lm <- lm(log(heart) ~ log(weight), data=cfseal); summary(cfseal.lm)
plot(log(heart) ~ log(weight), data = cfseal, pch=16, xlab = "Heart Weight (g, log scale)", 
ylab = "Body weight (kg, log scale)", axes=FALSE)
heartaxis <- 100*(2^seq(0,3))
bodyaxis <- c(20,40,60,100,180)
axis(1, at = log(bodyaxis), lab = bodyaxis)
axis(2, at = log(heartaxis), lab = heartaxis)
box()
abline(cfseal.lm)
}
\keyword{datasets}
