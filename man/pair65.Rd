\name{pair65}
\alias{pair65}
\non_function{}
\title{Heated Elastic Bands}
\usage{pair65}
\description{
The \code{pair65} data frame has 9 rows and 2 columns.
Eighteen elastic bands were divided into nine pairs, with bands
of similar stretchiness placed in the same pair. One member of
each pair was placed in hot water (60-65 degrees C) for four
minutes, while the other was left at ambient temperature.  After
a wait of about ten minutes, the amounts of stretch, under a 1.35 kg
weight, were recorded.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{heated}{a numeric vector giving the stretch lengths for
the heated bands}
    \item{ambient}{a numeric vector giving the stretch lengths for
the unheated bands}
  }
}
\source{
J.H. Maindonald
}
\examples{
mean(pair65$heated - pair65$ambient)
sd(pair65$heated - pair65$ambient)
}
\keyword{datasets}
