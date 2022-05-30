\name{Cars93.summary}
\alias{Cars93.summary}
\docType{data}
\title{A Summary of the Cars93 Data set}
\usage{Cars93.summary}
\description{
The \code{Cars93.summary} data frame has 6 rows and 4 columns
created from information in the \code{Cars93} data set in the Venables 
and Ripley MASS package. Each row corresponds to a different
class of car (e.g. Compact, Large, etc.).
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Min.passengers}{minimum
passenger capacity for each class of car}
    \item{Max.passengers}{maximum
passenger capacity for each class of car}
    \item{No.of.cars}{number of
cars in each class}
    \item{abbrev}{a factor with levels
    \code{C} Compact, \code{L} Large, 
    \code{M} Mid-Size, \code{Sm} Small, 
    \code{Sp} Sporty, \code{V} Van}
  }
}
\source{
Lock, R. H. (1993) 1993 New Car Data. Journal of Statistics
     Education 1(1)
}
\references{
MASS library
}
\examples{
type <- Cars93.summary$abbrev
type <- Cars93.summary[,4]
type <- Cars93.summary[,"abbrev"]
type <- Cars93.summary[[4]] # Take the object that is stored
                            # in the fourth list element.
type
pause()

attach(Cars93.summary)
  # R can now access the columns of Cars93.summary directly
abbrev
detach("Cars93.summary")
pause()

#  To change the name of the \verb!abbrev! variable (the fourth column)
names(Cars93.summary)[4] <- "code"
pause()

#  To change all of the names, try
names(Cars93.summary) <- c("minpass","maxpass","number","code")

}
\keyword{datasets}
