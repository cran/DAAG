\name{wages1833}
\alias{wages1833}
\non_function{}
\title{Wages of Lancashire Cotton Factory Workers in 1833}
\usage{wages1833}
\description{
The \code{wages1833} data frame gives the wages 
  of Lancashire cotton factory workers in 1833. 
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{age}{age in years}
    \item{mnum}{number of male workers}
    \item{mwage}{average wage of male workers}
    \item{fnum}{number of female workers}
    \item{fwage}{average wage of female workers}
  }
}
\source{
Boot, H.M. 1995. How Skilled Were the Lancashire Cotton Factory
  Workers in 1833? Economic History Review 48: 283-303.
}
\examples{
attach(wages1833)
plot(mwage~age,ylim=range(c(mwage,fwage[fwage>0])))
points(fwage[fwage>0]~age[fwage>0],pch=15,col="red")
lines(lowess(age,mwage))
lines(lowess(age[fwage>0],fwage[fwage>0]),col="red")
}
\keyword{datasets}
