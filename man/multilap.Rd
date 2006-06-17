\name{multilap}
\alias{multilap}
\title{Data Filtering Function}
\description{
A subset of data is selected for which the treatment to control
ratio of non-binary covariates is never outside a specified range.  
}
\usage{
multilap(df=nsw74psid1, maxf=20, colnames=c("educ", "age", "re74", "re75", 
"re78"))
}
\arguments{
  \item{df}{a data frame}
  \item{maxf}{filtering parameter}
  \item{colnames}{columns to be compared for filtering}
}
\author{J.H. Maindonald}

\keyword{models}
