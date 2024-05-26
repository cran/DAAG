\name{repPsych}
\alias{repPsych}
\docType{data}
\title{
Summary results from Reproducibility Study: Psychology
}
\description{
The chief interest, in collating this dataset, was in the
measures of effect size, for the originl study and for the
replication.
}
\usage{data("repPsych")}
\format{
  A data frame with 97 observations on the following 12 variables.
  \describe{
    \item{\code{stat}}{Test statistic. Character}
    \item{\code{Journal}}{Where published. Character.}
    \item{\code{Discipline}}{Cognitive or Social. Character.}
    \item{\code{reportedP.O}}{Reported p-value. Character.}
    \item{\code{effSizeO}}{Original effect size. Character.}
    \item{\code{T_r.O}}{Original effect size, as correlation. Numeric.}
    \item{\code{T_r.R}}{Replication effect size, as correlation. Numeric.}
    \item{\code{efftype}}{a character vector}
    \item{\code{tlike}}{Was test statistic t or F(1, m). Logical.}
    \item{\code{d_O}}{Original effect size, on Cohen's d scale. Numeric.}
    \item{\code{d_R}}{Replication effect size, on Cohen's d scale. Numeric.}
  }
}
\details{
Effect estimates on a correlation scale were converted to a Cohen's
\code{d} scale using \code{d} = \code{2r}/\code{sqrt(1-r^2)}.
}
\source{
\url{https://osf.io/fgjvw/}
}
\references{
\url{https://osf.io/ezum7/}
\url{https://osf.io/z7aux}
Open Science Collaboration, 2015. Estimating the reproducibility of psychological science. Science, 349(6251), p.aac4716.
}
\examples{
data(repPsych)
}
\keyword{datasets}
