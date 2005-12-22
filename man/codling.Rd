\name{codling}
\alias{codling}
\docType{data}
\title{Dose-mortality data, for fumigation of codling moth with methyl bromide}
\description{
  Data are from trials that studied the mortality response of codling moth
  to fumigation with methyl bromide.
}
\usage{data(codling)}
\format{
  A data frame with 99 observations on the following 10 variables.
  \describe{
    \item{dose}{Injected dose of methyl bromide, in gm per cubic meter}
    \item{tot}{Number of insects in chamber}
    \item{dead}{Number of insects dying}
    \item{pobs}{Proportion dying}
    \item{cm}{Control mortality, i.e., at dose 0}
    \item{ct}{Concentration-time sum}
    \item{Cultivar}{a factor with levels \code{BRAEBURN} \code{FUJI} \code{GRANNY} \code{Gala} \code{ROYAL} \code{Red Delicious} \code{Splendour}}
    \item{gp}{a factor which has a different level for each different
combination of \code{Cultivar}, \code{year} and \code{rep} (replicate).}
    \item{year}{a factor with levels \code{1988} \code{1989}}
    \item{numcm}{a numeric vector: total number of control insects}
  }
}
\details{
    The research that generated these data was in part funded by New Zealand
  pipfruit growers.  The published analysis was funded by New Zealand
  pipfruit growers. See also \code{sorption}.
 }
\source{
  Maindonald, J.H.; Waddell, B.C.; Petry, R.J. 2001.
  Apple cultivar effects on codling moth (Lepidoptera: Tortricidae)
  egg mortality following fumigation with methyl bromide.
  Postharvest Biology and Technology 22: 99-110.
}
\keyword{datasets}
