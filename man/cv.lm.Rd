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
cv.lm(df = houseprices, m = 3, form.lm = formula(sale.price ~ area), dots = FALSE,
 seed = 29)
}
\arguments{
  \item{df}{a data frame}
  \item{m}{the number of folds}
  \item{form.lm}{a formula object}
  \item{dots}{uses pch=16 for the plotting character}
  \item{seed}{random number generator seed}
}
\value{
For each fold, a table listing

\item{ } the explanatory variable values
\item{ } the predicted values
\item{ } the observed values
\item{ } the residuals

ms = the overall mean square of prediction error 

}
\author{J.H. Maindonald}

\seealso{ \code{lm} }

\examples{
data(houseprices)
cv.lm()
}
\keyword{models}
