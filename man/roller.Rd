\name{roller}
\alias{roller}
\non_function{}
\title{Lawn Roller Data}
\usage{data(roller)}
\description{
The \code{roller} data frame has 10 rows and 2 columns.
Different weights of roller were rolled over different parts
of a lawn, and the depression was recorded.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{weight}{a numeric vector consisting of the roller weights}
    \item{depression}{the depth of the depression made in the grass
under the roller}
  }
}
\source{
Stewart, K.M., Van Toor, R.F., Crosbie,
S.F. 1988. Control of grass grub (Coleoptera: Scarabaeidae) with
rollers of different design. N.Z. Journal of Experimental Agriculture
16: 141-150.
}
\examples{
data(roller)
plot(roller)
roller.lm <- lm(depression ~ weight, data = roller)
plot(roller.lm, which = 4)
}
\keyword{datasets}
