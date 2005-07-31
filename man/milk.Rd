\name{milk}
\alias{milk}
\non_function{}
\title{Milk Sweetness Study}
\usage{milk}
\description{
The \code{milk} data frame has 17 rows and 2 columns.
Each of 17 panelists compared two milk samples
for sweetness. 
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{four}{a numeric vector consisting of the assessments for four units
of additive}
    \item{one}{a numeric vector  while the is the assessment for one unit
of additive}
  }
}
\source{
 ??
}
\references{
??
}
\examples{
print("Rug Plot - Example 1.8.1")
xyrange <- range(milk)
plot(four ~ one, data = milk, xlim = xyrange, ylim = xyrange, pch = 16)
rug(milk$one)
rug(milk$four, side = 2)
abline(0, 1)
}
\keyword{datasets}
