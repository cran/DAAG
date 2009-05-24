\name{CVbinary}
\alias{CVbinary}
\alias{cv.binary}
\title{Cross-Validation for Regression with a Binary Response}
\description{
  This function gives training (internal) and cross-validation measures
  of predictive accuracy for regression with a binary response.  The
  data 
  are randomly divided between a number of `folds'.  Each fold is
  removed, in turn, while the remaining data are used to re-fit the
  regression model and to predict at the omitted observations.
}
\usage{
CVbinary(obj=frogs.glm, rand=NULL, nfolds=10, print.details=TRUE)

cv.binary(obj=frogs.glm, rand=NULL, nfolds=10, print.details=TRUE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{obj}{a \code{glm} object}
  \item{rand}{a vector which assigns each observation to a fold}
  \item{nfolds}{ the number of folds}
  \item{print.details}{ logical variable (TRUE = print detailed output, 
the default)}
}
\value{
  \item{cvhat}{predicted values from cross-validation}
  \item{internal}{internal or (better) training predicted values}  
  \item{training}{training predicted values}  
  \item{acc.cv}{cross-validation estimate of accuracy}
  \item{acc.internal}{internal or (better) training estimate of accuracy}  
  \item{acc.training}{training estimate of accuracy}  
}
\note{The term \sQuote{training} seems preferable to the term
  \sQuote{internal} in connection with predicted values, and
  the accuracy measure, that are based on the observations used to derive
  the model.}
\author{J.H. Maindonald}
\seealso{\code{\link{glm}}}
\examples{
frogs.glm <- glm(pres.abs ~ log(distance) + log(NoOfPools), 
                 family=binomial,data=frogs)
CVbinary(frogs.glm)
mifem.glm <- glm(outcome ~ ., family=binomial, data=mifem)
CVbinary(mifem.glm)
}
\keyword{models}
