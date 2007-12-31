\name{monica}
\alias{monica}
\title{WHO Monica Data}
\usage{monica}
\description{
The \code{monica} data frame has 6357 rows and 12 columns. Note
that \code{mifem} is the female subset of this data frame.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{outcome}{mortality outcome, a factor with levels \code{live}, 
\code{dead}} 
    \item{age}{age at onset}
    \item{sex}{m = male, f = female}
    \item{hosp}{y = hospitalized, n = not hospitalized}
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
  project; see the web site http://www.ktl.fi
}
\examples{
print("CART - Example 10.7")
summary(monica)
pause()

require(rpart)
monica.rpart <- rpart(outcome ~ ., data = monica, cp = 0.0025)
plotcp(monica.rpart)
printcp(monica.rpart)
pause()

monicab.rpart <- prune(monica.rpart, cp=0.006)
print(monicab.rpart)
}
\keyword{datasets}
