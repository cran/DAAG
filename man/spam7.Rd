\name{spam7}
\alias{spam7}
\title{Spam E-mail Data}
\usage{spam7}
\description{
The data consist of 4601 email items, of which 1813 items were identified
as spam.  This is a subset of the full dataset, with six only of the
57 explanatory variables in the complete dataset.
}
\format{
  Columns included are:
  \describe{
    \item{crl.tot}{total length of uninterrupted sequences of capitals}
    \item{dollar}{Occurrences of `\$', as percent of total number of characters}
    \item{bang}{Occurrences of `!', as percent of total number of characters}
    \item{money}{Occurrences of `money', as percent of total number of words}
    \item{n000}{Occurrences of the string `000', as percent of total
      number of words}
    \item{make}{Occurrences of `make', as \% of total number of words}
    \item{yesno}{outcome variable, a factor with levels
    \code{n} not spam,
    \code{y} spam}
  }
}
\source{
 George Forman, Hewlett-Packard Laboratories

The complete dataset, and documentation, are available from
\href{https://archive.ics.uci.edu/ml/datasets/spambasehttps://archive.ics.uci.edu/ml/datasets/spambase/}{Spam database}
}
\examples{
require(rpart)
spam.rpart <- rpart(formula = yesno ~ crl.tot + dollar + bang +
   money + n000 + make, data=spam7)
plot(spam.rpart)
text(spam.rpart)

}
\keyword{datasets}
