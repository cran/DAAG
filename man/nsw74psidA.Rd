\name{nsw74psidA}
\alias{nsw74psidA}
\title{A Subset of the nsw74psid1 Data Set}
\usage{nsw74psidA}
\description{
The \code{nsw74psidA} data frame has 252 rows and 10 columns.
See \code{nsw74psid1} for more information.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{trt}{a numeric vector}
    \item{age}{a numeric vector}
    \item{educ}{a numeric vector}
    \item{black}{a numeric vector}
    \item{hisp}{a numeric vector}
    \item{marr}{a numeric vector}
    \item{nodeg}{a numeric vector}
    \item{re74}{a numeric vector}
    \item{re75}{a numeric vector}
    \item{re78}{a numeric vector}
  }
}
\details{
This data set was obtained using:

\code{here <- age <= 40 & re74<=5000 & re75 <= 5000 & re78 < 30000}
\code{nsw74psidA <- nsw74psid1[here, ]}
}
\examples{
table(nsw74psidA$trt)

A1.lm <- lm(re78 ~ trt + (age + educ + re74 + re75) + (black +
      hisp + marr + nodeg), data = nsw74psidA)
summary(A1.lm)$coef

discA.glm <- glm(formula = trt ~ age + educ + black + hisp +
  marr + nodeg + re74 + re75, family = binomial, data = nsw74psidA)
A.scores <- predict(discA.glm)

options(digits=4)
overlap <- A.scores > -3.5 & A.scores < 3.8
A.lm <- lm(re78 ~ trt + A.scores, data=nsw74psidA, subset = overlap)
summary(A.lm)$coef

}
\keyword{datasets}
