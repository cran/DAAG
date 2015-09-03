\name{compareTreecalcs}
\alias{compareTreecalcs}
\title{Error rate comparisons for tree-based classification}
\description{Compare error rates, between different functions and different
  selection rules, for an approximately equal random division of the
  data into a training and test set.
}
\usage{
compareTreecalcs(x = yesno ~ ., data = DAAG::spam7, cp = 0.00025, fun = c("rpart",
"randomForest"))
}
\arguments{
  \item{x}{model formula}
  \item{data}{an data frame in which to interpret the variables
          named in the formula}
	\item{cp}{setting for the cost complexity parameter \code{cp},
	  used by rpart()}
  \item{fun}{one or both of "rpart" and "randomForest"}
}
\details{
  Data are randomly divided into two subsets, I and II. The function(s)
  are used in the standard way for calculations on subset I, and error
  rates returined that come from the calculations carried out by the
  function(s).  Predictions are made for subset II, allowing the
  calculation of a completely independent set of error rates.
}
\value{
  If \code{rpart} is specified in \code{fun}, the following:
  
\item{rpSEcvI}{the estimated cross-validation error rate
  when \code{rpart()} is run on the training data (I), and the
  one-standard error rule is used}
\item{rpcvI}{the estimated cross-validation error rate when 
 \code{rpart()} is run on subset I, and the model used that
  gives the minimum cross-validated error rate}
\item{rpSEtest}{the error rate when the model that leads to \code{rpSEcvI}
  is used to make predictions for subset II}
\item{rptest}{the error rate when the model that leads to \code{rpcvI}
  is used to make predictions for subset II}
\item{nSErule}{number of splits required by the one standard error rule}
\item{nREmin}{number of splits to give the minimum error}

  If \code{rpart} is specified in \code{fun}, the following:
\item{rfcvI}{the out-of-bag (OOB) error rate when 
 \code{randomForest()} is run on subset I}
\item{rftest}{the error rate when the model that leads to \code{rfcvI}
  is used to make predictions for subset II}
}
\author{John Maindonald}
\keyword{models}
