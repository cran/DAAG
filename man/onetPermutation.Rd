\name{onetPermutation}
\alias{onetPermutation}
\title{One Sample Permutation t-test}
\description{
This function computes the p-value for the one sample
t-test using a permutation test.  The permutation
density can  also be plotted.
}
\usage{
onetPermutation(x=pair65$heated - pair65$ambient, nsim=2000, plotit=TRUE)
}
\arguments{
  \item{x}{a numeric vector containing the sample values (centered
at the null hypothesis value) }
  \item{nsim}{the number of permutations (randomly selected)}
  \item{plotit}{if TRUE, the permutation density is plotted }
}
\value{
The p-value for the test of the hypothesis that the mean of \code{x}
differs from 0
}
\references{ 
Good, P. 2000. Permutation Tests. Springer, New York.
}
\author{J.H. Maindonald}
\examples{
onetPermutation()
}
\keyword{models}
