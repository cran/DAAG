\name{bestsetNoise}
\alias{bestsetNoise}
\alias{bestset.noise}
\alias{bsnCV}
\title{Best Subset Selection Applied to Noise}
\description{
Best subset selection applied to completely random noise.  This
function demonstrates how variable selection techniques in 
regression can often err in suggesting that more variables be
included in a regression model than necessary.
}
\usage{
bestsetNoise(m=100, n=40, method="exhaustive", nvmax=3, print.summary = TRUE)

bestset.noise(m=100, n=40, method="exhaustive", nvmax=3, print.summary = TRUE)

bsnCV(m = 100, n = 40, method = "exhaustive", nvmax = 3, 
      nfolds = 2, print.summary = TRUE) 
}
\arguments{
  \item{m}{the number of observations to be simulated. }
  \item{n}{the number of predictor variables in the simulated
    model. }
  \item{method}{Use \code{exhaustive} search, or \code{backward} selection, 
      or \code{forward} selection, or \code{sequential} replacement.}
    \item{nvmax}{maximum number of explanatory variables in model.}
    \item{nfolds}{For splitting the data into training and text sets,
      the number of folds.}
    \item{print.summary}{Should summary information be printed}
}
\details{
A set of \code{n} predictor variables are simulated as independent
standard normal variates, in addition to a response variable which
is also independent of the predictors.  The best model with
\code{nvmax} variables is selected using the \code{regsubsets()}
function from the leaps package.  (The leaps package must be installed
for this function to work.)

The function \code{bsnCV} splits the data (randomly) into \code{nfolds}
(2 or more) parts.  It puts each part aside in turn for use to fit
the model (effectively, test data), with the remaining data used
for selecting the variables that will be used for fitting. One model
fit is returned for each of the \code{nfolds} parts.
}
\value{
  \code{bestsetNoise} returns the \code{lm} model object for the "best"
  model.
  
\code{bsnCV} returns as many models as there are folds. 
}
\author{J.H. Maindonald}

\seealso{ \code{\link{lm}}}

\examples{
leaps.out <- try(require(leaps, quietly=TRUE))
leaps.out.log <- is.logical(leaps.out)
if ((leaps.out.log==TRUE)&(leaps.out==TRUE)){
bestsetNoise(20,6) # `best' 3-variable regression for 20 simulated observations 
                   # on 7 unrelated variables (including the response)
bsnCV(20,6) # `best' 3-variable regressions (one for each fold) for 20 
                   # simulated observations on 7 unrelated variables
                   # (including the response)
}
}
\keyword{models}
