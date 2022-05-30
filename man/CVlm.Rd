\name{CVlm}
\alias{CVlm}
\alias{cv.lm}
\title{Cross-Validation for Linear Regression}
\description{
This function gives internal and cross-validation measures of predictive
accuracy for multiple linear regression. (For binary logistic
regression, use the \code{CVbinary} function.) The data are
randomly assigned to a number of `folds'.
Each fold is removed, in turn, while the remaining data is used
to re-fit the regression model and to predict at the deleted observations.
}
\usage{
CVlm(data = DAAG::houseprices, form.lm = formula(sale.price ~ area),
              m = 3, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
              col.folds=NULL,               
              main="Small symbols show cross-validation predicted values",
              legend.pos="topleft", 
              printit = TRUE, ...)
cv.lm(data = DAAG::houseprices, form.lm = formula(sale.price ~ area),
              m = 3, dots = FALSE, seed = 29, plotit = c("Observed","Residual"),
              col.folds=NULL,               
              main="Small symbols show cross-validation predicted values",
              legend.pos="topleft", printit = TRUE, ...)
              }
\arguments{
  \item{data}{a data frame}
  \item{form.lm}{a formula or \code{lm} call or \code{lm} object}
  \item{m}{the number of folds}
  \item{dots}{uses pch=16 for the plotting character}
  \item{seed}{random number generator seed}
  \item{plotit}{This can be one of the text strings \code{"Observed"},
    \code{"Residual"}, or a logical value.  The logical \code{TRUE} is
    equivalent to \code{"Observed"}, while \code{FALSE} is equivalent
    to \code{""} (no plot)}
  \item{col.folds}{Per fold color settings}
  \item{main}{main title for graph}
  \item{legend.pos}{position of legend: one of
    \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"},
	  \code{"left"}, \code{"topleft"}, \code{"top"}, \code{"topright"},
          \code{"right"}, \code{"center"}.
  }  
  \item{printit}{if TRUE, output is printed to the screen}
  \item{\dots}{Other arguments, to be passed through to the function \code{legend()}}
}
\details{
  When \code{plotit="Residual"} and there is more than one explanatory
  variable, the fitted lines that are shown for the individual folds
  are approximations.
  }
  \value{The input data frame is returned, with additional columns
    \code{Predicted} (Predicted values using all observations)
    and \code{cvpred} (cross-validation predictions). The
cross-validation residual sum of squares (\code{ss}) and
degrees of freedom (\code{df}) are returned as attributes of
the data frame.
}
\author{J.H. Maindonald}
\seealso{\code{\link{lm}}, \code{\link{CVbinary}}}
\examples{
CVlm()
\dontrun{
CVlm(data=nihills, form.lm=formula(log(time)~log(climb)+log(dist)),
          plotit="Observed")
CVlm(data=nihills, form.lm=formula(log(time)~log(climb)+log(dist)),
     plotit="Residual")
out <- CVlm(data=nihills, form.lm=formula(log(time)~log(climb)+log(dist)),
               plotit="Observed")
out[c("ms","df")]
}
}
\keyword{models}
