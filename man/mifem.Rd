\name{mifem}
\alias{mifem}
\non_function{}
\title{Mortality Outcomes for Females Suffering Myocardial Infarction}
\usage{mifem}
\description{
The \code{mifem} data frame has 1295 rows and 10 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{outcome}{mortality outcome, a factor with levels \code{live}, 
\code{dead}} 
    \item{age}{age at onset}
    \item{yronset}{year of onset}
    \item{premi}{previous myocardial infarction event, a 
factor with levels \code{y}, \code{n}, \code{nk} not known }
    \item{smstat}{smoking status, a factor with levels \code{c} 
current, \code{x} ex-smoker, \code{n} non-smoker, \code{nk} not known}
    \item{diabetes}{a factor with levels \code{y}, \code{n}, \code{nk} not 
known}
    \item{highbp}{high blood pressure, a factor with levels
\code{y}, \code{n}, \code{nk} not known }
    \item{hichol}{high cholesterol, a factor with levels
\code{y}, \code{n} \code{nk} not known}
    \item{angina}{a factor with levels \code{y},
\code{n}, \code{nk} not known}
    \item{stroke}{a factor with levels
\code{y}, \code{n}, \code{nk} not known}
  }
}
\source{
 Newcastle (Australia) centre of the Monica
  project; see the web site http://www.ktl.fi/monicaindex.html
}
\examples{
print("CART - Example 10.7")
summary(mifem)
pause()

require(rpart)
mifem.rpart <- rpart(outcome ~ ., data = mifem, cp = 0.0025)
plotcp(mifem.rpart)
printcp(mifem.rpart)
pause()

mifemb.rpart <- prune(mifem.rpart, cp=0.006)
print(mifemb.rpart)
}
\keyword{datasets}
