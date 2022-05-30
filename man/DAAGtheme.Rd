\name{DAAGtheme}
\alias{DAAGtheme}
\title{
Function to generate lattice themes for graphs.
}
\description{
This generates themes for use in a planned 4th edition of 
"Data Analysis and Graphics Using R".
}
\usage{
DAAGtheme(fontsize = list(text = 10, points = 6), box = "gray40",
          sides = list(tck = 0.6, pad1 = 0.75, pad2 = 0.75),...)
}
\arguments{
  \item{fontsize}{
Fontize for text and points. Specify as, e.g., \code{list(text = 10, points = 6)}.
}
\item{box}{Color for the panel and strip borders.}
\item{sides}{List, with elements \code{tck} (Tick length, as fraction of lattice default),
and margin paddings \code{pad1}, \code{pad2}, \code{pad3}, and \code{pad4}.
Margin paddings set the distance, in lines, from the tick marks to the tick labels.
}
\item{\dots}{
Settings that will be passed to \code{simpleTheme()}.
}
}
\details{
Setting the color of the bounding box and of the strip boxes set gray, 
which is the default, reduces the focus on them.
}
\value{
A list which can be used as the \code{par.settings} argument to lattice 
graphics functions, or as the \code{theme} argument to \code{trellis.par.set()}.
}
\author{
John Maindonald.
}
\note{
The code provides an example of the creation of a functions that 
generates themes that are tuned to specific user requirements.
In this connection, see also \code{\link{theEconomist.theme}}.
}
\seealso{
\code{\link{standard.theme}}, \code{\link{simpleTheme}},
\code{\link{theEconomist.theme}},
\code{\link{custom.theme}}
}
\examples{
bwtheme <- DAAGtheme(pch=2:4)
lattice::xyplot(csoa ~ age | target, groups=sex, 
                data=DAAG::tinting, par.settings=bwtheme)
}
\keyword{graphics}

