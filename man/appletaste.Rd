\name{appletaste}
\alias{appletaste}
\docType{data}
\title{Tasting experiment that compared four apple varieties}
\description{
  Each of 20 tasters each assessed three out of the four varieties.
  The experiment was conducted according to a balanced incomplete
  block design.
}
\usage{data(appletaste)}
\format{
  A data frame with 60 observations on the following 3 variables.
  \describe{
    \item{aftertaste}{a numeric vector} Apple samples were rated for
    \code{aftertaste}, by making a mark on a continuous scale that
    ranged from 0 (extreme dislike) to 150 (like very much).
    \item{panelist}{a factor with levels \code{a} \code{b} \code{c} \code{d} \code{e} \code{f} \code{g} \code{h} \code{i} \code{j} \code{k} \code{l} \code{m} \code{n} \code{o} \code{p} \code{q} \code{r} \code{s} \code{t}}
    \item{product}{a factor with levels \code{298} \code{493} \code{649} \code{937}}
  }
}
\examples{
data(appletaste)
appletaste.aov <- aov(aftertaste ~ panelist + product, data=appletaste)
termplot(appletaste.aov)
}
\keyword{datasets}
