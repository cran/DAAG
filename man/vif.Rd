\name{vif}
\alias{vif}
\title{Variance Inflation Factors}
\description{
Variance inflation factors are computed for the standard
errors of linear model coefficient estimates.
}
\usage{
vif(obj, digits=5)
}
\arguments{
  \item{obj}{ A \code{lm} object }
  \item{digits}{ Number of digits}
}
\value{
A vector of variance inflation factors corresponding to 
the coefficient estimates given in the \code{lm} object.
}
\author{J.H. Maindonald}
\seealso{\code{lm}}

\examples{
data(litters)
litters.lm <- lm(brainwt ~ bodywt + lsize, data = litters)
vif(litters.lm)

data(carprice)
carprice1.lm <- lm(gpm100 ~ Type+Min.Price+Price+Max.Price+Range.Price,
    data=carprice)
vif(carprice1.lm)

carprice.lm <- lm(gpm100 ~ Type + Price, data = carprice)
vif(carprice1.lm)

}
\keyword{models}
