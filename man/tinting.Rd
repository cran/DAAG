\name{tinting}
\alias{tinting}
\title{Car Window Tinting Experiment Data}
\usage{tinting}
\description{
These data are from an experiment that aimed to model the
effects of the tinting of car windows on visual
performance. The authors
were mainly interested in effects on side window vision, and hence in
visual recognition tasks that would be performed when looking through
side windows.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{case}{observation number}
    \item{id}{subject identifier code (1-26)}
    \item{age}{age (in years) }
    \item{sex}{a factor with levels
\code{f} female,
\code{m} male}
    \item{tint}{an ordered factor with levels representing degree of
tinting: \code{no} < \code{lo} < \code{hi}}
    \item{target}{a factor with levels
   \code{locon}: low contrast,
   \code{hicon}: high contrast }
    \item{it}{the inspection time, the time required
to perform a simple discrimination task (in milliseconds)}
    \item{csoa}{critical stimulus onset asynchrony, the time
to recognize an alphanumeric target (in milliseconds)}
    \item{agegp}{a factor with levels
\code{younger}, 21-27,
\code{older}, 70-78 }
  }
}
\details{
Visual light transmittance (VLT) levels were 100\% (tint=none),
81.3\% (tint=lo), and 35.1\% (tint=hi).  Based on these and other
data, Burns et al. argue that road safety may be compromised if
the front side windows of cars are tinted to 35% VLT.
}
\source{
 Burns, N.R., Nettlebeck, T., White, M. and Willson, J., 1999.
Effects of car window tinting on visual performance: a comparison of
younger and older drivers. Ergonomics 42: 428-443.
}
\examples{
library(lattice)
levels(tinting$agegp) <- capstring(levels(tinting$agegp))
xyplot(csoa ~ it | sex * agegp, data=tinting) # Simple use of xyplot()
pause()

xyplot(csoa ~ it|sex*agegp, data=tinting, panel=panel.superpose, groups=target)
pause()

xyplot(csoa ~ it|sex*agegp, data=tinting, panel=panel.superpose, col=1:2,
  groups=target, key=list(x=0.14, y=0.84, points=list(pch=rep(1,2),
  col=1:2), text=list(levels(tinting$target), col=1:2), border=TRUE))
\dontrun{
xyplot(csoa ~ it|sex*agegp, data=tinting, panel=panel.superpose,
  groups=tint, type=c("p","smooth"), span=0.8, col=1:3,
  key=list(x=0.14, y=0.84, points=list(pch=rep(1,2), col=1:3),
  text=list(levels(tinting$tint), col=1:3), border=TRUE))
  }
}
\keyword{datasets}
