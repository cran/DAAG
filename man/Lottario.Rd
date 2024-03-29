\name{Lottario}
\alias{Lottario} 
\docType{data}
\title{Ontario Lottery Data} 
\usage{Lottario} 
\description{The data frame \code{Lottario}
is a summary of 122 weekly draws of an Ontario lottery, beginning in
November, 1978.  Each draw consists of 7 numbered balls, drawn without
replacement from an urn consisting of balls numbered from 1 through 39.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Number}{the integers from 1 to 39, representing the
numbered balls}
    \item{Frequency}{the number of occurrences of each numbered ball}
}
} 
  \source{ 
The Ontario Lottery Corporation
} 
  \references{
Bellhouse, D.R. (1982). Fair is fair:  new rules for Canadian lotteries.
Canadian Public Policy - Analyse de Politiques 8: 311-320.
} 
\examples{ 
order(Lottario$Frequency)[33:39]  # the 7 most frequently chosen numbers
} 
\keyword{datasets}
