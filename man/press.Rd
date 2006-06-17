\name{press}
\alias{press}
\title{Predictive Error Sum of Squares}
\description{
Allen's PRESS statistic is computed for a fitted model.
}
\usage{
press(obj)
}
\arguments{
  \item{obj}{ A \code{lm} object }
}
\value{
A single numeric value.
}
\author{W.J. Braun}
\seealso{\code{lm}}

\examples{
litters.lm <- lm(brainwt ~ bodywt + lsize, data = litters)
press(litters.lm)
litters.lm0 <- lm(brainwt ~ bodywt + lsize -1, data=litters)
press(litters.lm0) # no intercept
litters.lm1 <- lm(brainwt ~ bodywt, data=litters)
press(litters.lm1) # bodywt only
litters.lm2 <- lm(brainwt ~ bodywt + lsize + lsize:bodywt, data=litters)
press(litters.lm2) # include an interaction term
}
\keyword{models}
