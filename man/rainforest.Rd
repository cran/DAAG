\name{rainforest}
\alias{rainforest}
\non_function{}
\title{Rainforest Data}
\usage{data(rainforest)}
\description{
The \code{rainforest} data frame has 65 rows and 7 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{dbh}{a numeric vector}
    \item{wood}{a numeric vector}
    \item{bark}{a numeric vector}
    \item{root}{a numeric vector}
    \item{rootsk}{a numeric vector}
    \item{branch}{a numeric vector}
    \item{species}{a factor with levels
\code{Acacia mabellae},
\code{C. fraseri},
\code{Acmena smithii},
\code{B. myrtifolia} }
  }
}
\source{
J. Ash, Australian National University
}
\examples{
data(rainforest)
table(rainforest$species)
}
\keyword{datasets}
