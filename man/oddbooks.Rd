\name{oddbooks}
\alias{oddbooks}
\docType{data}
\title{Measurements on 12 books}
\description{
  Data giving thickness (mm), height (cm), width (cm)
  and weight (g), of 12 books.  Books were selected so that thickness
decreased as page area increased
}
\usage{data(oddbooks)}
\format{
  A data frame with 12 observations on the following 4 variables.
  \describe{
    \item{thick}{a numeric vector}
    \item{height}{a numeric vector}
    \item{breadth}{a numeric vector}
    \item{weight}{a numeric vector}
  }
}
\details{
}
\source{
JM took books from his library.
}
\examples{
data(oddbooks)
str(oddbooks)
plot(oddbooks) 
}
\keyword{datasets}
