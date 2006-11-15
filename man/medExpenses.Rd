\name{medExpenses}
\alias{medExpenses}
\title{Family Medical Expenses}
\usage{medExpenses}
\description{
The \code{medExpenses} data frame  
contains average weekly medical expenses including drugs for 33 families
randomly sampled from a community of 600 families which contained
2700 individuals.  These data were collected in the 1970's at an 
unknown location.
}
\format{
  \describe{
    \item{familysize}{number of individuals in a family}
    \item{expenses}{average weekly cost for medical expenses per
family member}
}
}
\examples{
with(medExpenses, weighted.mean(expenses, familysize))
}
\keyword{datasets}
