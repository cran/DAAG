\name{twot.permutation}
\alias{twot.permutation}
\title{Two Sample Permutation Test}
\description{
This function computes the p-value for the two sample
t-test using a permutation test.  The permutation density
can  also be plotted.
}
\usage{
twot.permutation(x1=two65$ambient, x2=two65$heated, nsim=2000, plotit=TRUE)
}
\arguments{
  \item{x1}{Sample 1}
  \item{x2}{Sample 2}
  \item{nsim}{Number of simulations}
  \item{plotit}{If TRUE, the permutation density will be plotted}
}
\details{
Suppose we have n1 values in one group and n2 in a
second, with n = n1 + n2. The permutation distribution
results from taking all possible samples of n2 values from
the total of n values.
}
\value{
The p-value for the test of the hypothesis that the mean of
\code{x1} differs from \code{x2}
}
\references{ 
Good, P. 2000. Permutation Tests. Springer, New York.
}
\author{J.H. Maindonald}

\examples{
data(two65)
twot.permutation()
}
\keyword{models}
