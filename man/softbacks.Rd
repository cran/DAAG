\name{softbacks}
\alias{softbacks}
\non_function{}
\title{Measurements on a Selection of Paperback Books}
\usage{data(softbacks)}
\description{
This is a subset of the \code{allbacks} data frame 
which gives measurements
on the volume and weight of 8 paperback books.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{volume}{a numeric vector giving the book volumes
in cubic centimeters}
    \item{weight}{a numeric vector giving the weights in grams}
  }
}
\source{
The bookshelf of J. H. Maindonald.
}
\examples{
data(softbacks)
print("Outliers in Simple Regression - Example 5.2")
paperback.lm <- lm(weight ~ volume, data=softbacks)
summary(paperback.lm)
plot(paperback.lm)
}
\keyword{datasets}
