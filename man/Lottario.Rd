\name{Lottario} 
\alias{Lottario} 
\non_function{} 
\title{Ontario Lottery Data} 
\usage{data(cuckoos)} 
\description{ The data frame \verb!Lottario!
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
data(Lottario)
order(Lottario$Frequency)[33:39]  # the 7 most frequently chosen numbers
} 
\keyword{datasets}
