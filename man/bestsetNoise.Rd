\name{bestsetNoise}
\alias{bestsetNoise}
\alias{bestset.noise}
\alias{bsnCV}
\alias{bsnOpt}
\alias{bsnVaryNvar}
\title{Best Subset Selection Applied to Noise}
\description{
Best subset selection applied to completely random noise.  This
function demonstrates how variable selection techniques in
regression can often err in including explanatory variables that
are indistinguishable from noise.
}
\usage{
bestsetNoise(m = 100, n = 40, method = "exhaustive", nvmax = 3,
              X = NULL, y=NULL, intercept=TRUE,
              print.summary = TRUE, really.big = FALSE, ...)

bestset.noise(m = 100, n = 40, method = "exhaustive", nvmax = 3,
              X = NULL, y=NULL, intercept=TRUE,
              print.summary = TRUE, really.big = FALSE, ...)

bsnCV(m = 100, n = 40, method = "exhaustive", nvmax = 3,
              X = NULL, y=NULL, intercept=TRUE, nfolds = 2,
              print.summary = TRUE, really.big = FALSE)

bsnOpt(X = matrix(rnorm(25 * 10), ncol = 10), y = NULL, method = "exhaustive",
              nvmax = NULL, nbest = 1, intercept = TRUE, criterion = "cp",
              tcrit = NULL, print.summary = TRUE, really.big = FALSE,
         ...)

bsnVaryNvar(m = 100, nvar = nvmax:50, nvmax = 3, method = "exhaustive",
              intercept=TRUE,
              plotit = TRUE, xlab = "# of variables from which to select",
              ylab = "p-values for t-statistics", main = paste("Select 'best'",
                                                  nvmax, "variables"),
              details = FALSE, really.big = TRUE, smooth = TRUE)
}
\arguments{
  \item{m}{the number of observations to be simulated, ignored if X is supplied.}
  \item{n}{the number of predictor variables in the simulated
    model, ignored if X is supplied.}
  \item{method}{Use \code{exhaustive} search, or \code{backward} selection,
      or \code{forward} selection, or \code{sequential} replacement.}
    \item{nvmax}{Number of explanatory variables in model.}
    \item{X}{Use columns from this matrix.  Alternatively, X may be a
  data frame, in which case a model matrix will be formed from it.
  If not \code{NULL}, \code{m} and \code{n} are ignored.}
\item{y}{If not supplied, random normal noise will be generated.}
\item{nbest}{Number of models, for each choice of number of columns
  of explanatory variables, to return (\code{bsnOpt}). If \code{tcrit}
  is non-NULL, it may be important to set this greater than one, in
  order to have a good chance of finding models with minimum absolute
  \emph{t}-statistic greater than \code{tcrit}.}
  \item{intercept}{Should an intercept be added?}
  \item{nvar}{range of number of candidate variables (\code{bsnVaryVvar}).}
    \item{nfolds}{For splitting the data into training and text sets,
      the number of folds.}
    \item{criterion}{Criterion to use in choosing between models with
      different numbers of explanatory variables (\code{bsnOpt}).
      Alternatives are \dQuote{bic}, or \dQuote{cip} or \dQuote{adjr2}.}
    \item{tcrit}{Consider only those models for which the minimum absolute
     \emph{t}-statistic is greater than \code{tcrit}.}
    \item{print.summary}{Should summary information be printed.}
\item{plotit}{
Plot a graph? (\code{bsnVaryVvar})}
  \item{xlab}{
\emph{x}-label for graph (\code{bsnVaryVvar})
}
  \item{ylab}{
\emph{y}-label for graph (\code{bsnVaryVvar}.)
}
  \item{main}{
main title for graph (\code{bsnVaryVvar}.)
}
\item{details}{Return detailed output list (\code{bsnVaryVvar})}
    \item{really.big}{Set to \code{TRUE} to allow (currently) for more than
      50 explanatory variables.}
    \item{smooth}{Fit smooth to graph? (\code{bsnVaryVvar}).}
    \item{...}{Additional arguments, to be passed through to
      \code{regsubsets()}.}
}
\details{
If \code{X} is not supplied, and in any case for \code{bsnVaryNvar}, a
set of \code{n} predictor variables are simulated as independent
standard normal, i.e. N(0,1), variates.  Additionally a N(0,1) response
variable is simulated.  The function \code{bsnOpt} selects the
\sQuote{best} model with \code{nvmax} or fewer explanatory variables,
where the argument \code{criterion} specifies the criterion that will
be used to choose between models with different numbers of explanatory
columns.  Other functions select the \sQuote{best} model with
\code{nvmax} explanatory columns.  In any case, the selection is made
using the \code{regsubsets()} function from the leaps package.
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
  model with \code{nvmax} explanatory columns.

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
  returned are for the \sQuote{best} model with nvmax explanatory
  variables.

  \code{bsnOpt} silently returns a list with elements:
  \item{u1}{\sQuote{best} model (\code{lm} object) with \code{nvmax} or
    fewer columns of predictors. If \code{tcrit} is non-NULL, and there
    is no model for which all coefficients have \emph{t}-statistics
    less than \code{tcrit} in absolute value, \code{u1} will be NULL.}
  \code{tcrit}{For each model, the minimum of the absolute values of
  the \emph{t}-statistics.}
  \code{regsubsets_obj}{The object returned by the call to \code{regsubsets}.}
  }

  \note{These functions are primarily designed to demonstrate the biases
    that can be expected, relative to theoretical estimates of standard
    errors of parameters and other fitted model statistics, when there
    is prior selection of the columns that are to be included in the
    model. With the exception of \code{bsnVaryNvar}, they can also be
    used with an \code{X} and \code{y} for actual data.  In that case,
    the \emph{p}-values should be compared with those
    obtained from repeated use of the function where \code{y} is random
    noise, as a check on the extent of selection effects.
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
bsnOpt()
}
}
\keyword{models}
