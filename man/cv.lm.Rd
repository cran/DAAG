\name{cv.lm}
\alias{cv.lm}
\title{Cross-Validation for Linear Regression}
\description{
This function gives internal and cross-validation measures of predictive
accuracy for ordinary linear regression.  The data are 
randomly assigned to a number of `folds'.  
Each fold is removed, in turn, while the remaining data is used
to re-fit the regression model and to predict at the deleted observations.
}
\usage{
cv.lm(df = houseprices, form.lm = formula(sale.price ~ area), m=3, dots = 
      FALSE, seed=29, plotit=TRUE, printit=TRUE)
}
\arguments{
  \item{df}{a data frame}
  \item{form.lm}{a formula object}
  \item{m}{the number of folds}
  \item{dots}{uses pch=16 for the plotting character}
  \item{seed}{random number generator seed}
  \item{plotit}{if TRUE, a plot is constructed on the active device}
  \item{printit}{if TRUE, output is printed to the screen}
}
\value{
\item{ss}{the cross-validation residual sum of squares}
\item{df}{degrees of freedom}
}
\author{J.H. Maindonald}
\seealso{ \code{lm} }
\examples{
cv.lm()
}
\keyword{models}
