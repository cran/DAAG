\name{bestset.noise}
\alias{bestset.noise}
\title{Best Subset Selection Applied to Noise}
\description{
Best subset selection applied to completely random noise.  This
function demonstrates how variable selection techniques in 
regression can often err in suggesting that more variables be
included in a regression model than necessary.
}
\usage{
bestset.noise(m=100, n=40)
}
\arguments{
  \item{m}{the number of observations to be simulated. }
  \item{n}{the number of predictor variables in the simulated
model. }
}
\details{
A set of \code{n} predictor variables are simulated as independent
standard normal variates, in addition to a response variable which
is also independent of the predictors.  The best three variable
model relating the response to the predictors is selected using
functions from the leaps package.  (The leaps package
must be installed in order for this function to work.)
}
\value{
\code{bestset.noise} returns a list obtained from the 
\code{\link{summary.lm}} function.
}
\author{J.H. Maindonald}

\seealso{ \code{\link{lm}}}

\examples{
leaps.out <- try(require(leaps, quietly=TRUE))
leaps.out.log <- is.logical(leaps.out)
if ((leaps.out.log==TRUE)&(leaps.out==TRUE))
bestset.noise(20,6) # `best' 3-variable regression for 20 simulated observations 
                    # on 7 unrelated variables (including the response)
}
\keyword{models}
