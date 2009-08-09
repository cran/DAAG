\name{onewayPlot}
\alias{onewayPlot}
\title{Display of One Way Analysis Results}
\description{
A line plot of estimates for unstructured comparison of factor levels
}
\usage{
onewayPlot(obj, trtnam = "trt", axisht = 6, xlim = NULL, 
    xlab = NULL, lsdht = 1.5, hsdht = 0.5, textht = axisht - 
        2.5, oma = rep(1, 4), angle = 80, alpha = 0.05)
}
\arguments{
  \item{obj}{One way analysis of variance object (from aov)}
  \item{trtnam}{name of factor for which line plot is required}
  \item{axisht}{Axis height}
  \item{xlim}{Range on horizontal axis}
  \item{xlab}{Horizontal axis label}
  \item{lsdht}{Height adjustment parameter for display of LSD}
  \item{hsdht}{Height adjustment parameter for display of Tukey's HSD} 
  \item{textht}{Height of text}
  \item{oma}{Outer margin area}
  \item{angle}{Text angle (in degrees)}
  \item{alpha}{Test size}
}
\value{
Estimates, labeled with level names, are set out along a line} 
\author{J.H. Maindonald}

\examples{
rice.aov <- aov(ShootDryMass ~ trt, data=rice)
onewayPlot(obj=rice.aov)
}
\keyword{models}
