\name{oneway.plot}
\alias{oneway.plot}
\title{Display of One Way Analysis Results}
\description{
A line plot of means for unstructured comparison.
}
\usage{
oneway.plot(obj = rice.aov, axisht = 6, xlim=NULL, xlab=NULL,
               lsdht = 1.5, hsdht = 0.5, textht=axisht-3,
               oma=rep(1,4), angle=80, alpha = 0.05)
}
\arguments{
  \item{obj}{One way analysis of variance object (from aov)}
  \item{axisht}{Axis height}
  \item{xlim}{Range on horizontal axis}
  \item{xlab}{Horizontal axis label}
  \item{lsdht}{Height adjustment parameter for LSD comparison plot}
  \item{hsdht}{Height adjustment parameter for Tukey's HSD comparison 
plot}
  \item{textht}{Height of text}
  \item{oma}{Outer margin area}
  \item{angle}{Text angle (in degrees)}
  \item{alpha}{Test size}
}
\value{
A line plot
}
\author{J.H. Maindonald}

\examples{
data(rice)
rice.aov <- aov(ShootDryMass ~ trt, data=rice)
oneway.plot(obj=rice.aov)
}
\keyword{models}
