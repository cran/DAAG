\name{bestsetNoise}
\alias{bestsetNoise}
\alias{bestset.noise}
\alias{bsnCV}
\alias{bsnVaryNvar}
\title{Best Subset Selection Applied to Noise}
\description{
Best subset selection applied to completely random noise.  This
function demonstrates how variable selection techniques in
regression can often err in suggesting that more variables be
included in a regression model than necessary.
}
\usage{
bestsetNoise(m=100, n=40, method="exhaustive", nvmax=3, X=NULL, print.summary = TRUE, really.big=FALSE)

bestset.noise(m=100, n=40, method="exhaustive", nvmax=3, X=NULL, print.summary = TRUE, really.big=FALSE)

bsnCV(m = 100, n = 40, method = "exhaustive", nvmax = 3, X=NULL,
      nfolds = 2, print.summary = TRUE, really.big=FALSE)

bsnVaryNvar(m = 100, nvar = nvmax:50, nvmax = 3, method = "exhaustive", plotit = TRUE, xlab = "# of variables from which to select", ylab = "p-values for t-statistics", main = paste("Select 'best'", nvmax, "variables"), details = FALSE, really.big = TRUE, smooth = TRUE)
}
\arguments{
  \item{m}{the number of observations to be simulated. }
  \item{n}{the number of predictor variables in the simulated
    model. }
  \item{method}{Use \code{exhaustive} search, or \code{backward} selection,
      or \code{forward} selection, or \code{sequential} replacement.}
    \item{nvmax}{maximum number of explanatory variables in model.}
  \item{X}{Use columns from the matrix that is supplied.  If not \code{NULL},
      \code{m} and \code{n} are ignored.}
  \item{nvar}{range of number of candidate variables (\code{bsnVaryVvar})}
    \item{nfolds}{For splitting the data into training and text sets,
      the number of folds.}
    \item{print.summary}{Should summary information be printed}
\item{plotit}{
Plot a graph? (\code{bsnVaryVvar})}
  \item{xlab}{
\emph{x}-label for graph (\code{bsnVaryVvar})
}
  \item{ylab}{
\emph{y}-label for graph (\code{bsnVaryVvar})
}
  \item{main}{
main title for graph (\code{bsnVaryVvar})
}
\item{details}{Return detailed output list (\code{bsnVaryVvar})}
    \item{really.big}{Set to \code{TRUE} to allow (currently) for more than
      50 explanatory variables.}
 \item{smooth}{Fit smooth to graph? (\code{bsnVaryVvar})}
}
\details{
If \code{X} is not supplied, and in any case for \code{bsnVaryNvar}, a
set of \code{n} predictor variables are simulated as independent
standard normal, i.e. N(0,1), variates.  Additionally a N(0,1) response
variable is simulated.  The best model with \code{nvmax} variables is
selected using the \code{regsubsets()} function from the leaps package.
(The leaps package must be installed for this function to work.)

The function \code{bsnCV} splits the data (randomly) into \code{nfolds}
(2 or more) parts.  It puts each part aside in turn for use to fit
the model (effectively, test data), with the remaining data used
for selecting the variables that will be used for fitting. One model
fit is returned for each of the \code{nfolds} parts.

The function \code{bsnVaryVvar} makes repeated calls to
\code{bestsetNoise}
}
\value{
  \code{bestsetNoise} returns the \code{lm} model object for the "best"
  model.

  \code{bsnCV} returns as many models as there are folds.

  \code{bsnVaryVvar} silently returns either (\code{details=FALSE}) a
  matrix that has \emph{p}-values of the coefficients for the \sQuote{best}
  choice of model for
    each different number of candidate variables, or
    (\code{details=TRUE}) a list with elements:
      \item{coef}{A matrix of sets of regression coefficients}
  \item{SE}{A matrix of standard errors}
  \item{pval}{A matrix of \emph{p}-values}
  Matrices have one row for each choice of \code{nvar}.  The statistics
  returned are for the \sQuote{best} model with nvmax explanatory variables.
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
bsnVaryNvar(m = 50, nvar = 3:6, nvmax = 3, method = "exhaustive",
            plotit=FALSE, details=TRUE)
}
}
\keyword{models}
