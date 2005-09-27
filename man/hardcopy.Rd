\name{hardcopy}
\alias{hardcopy}
\title{Graphical Output for Hardcopy}
\description{
This function streamlines graphical output to the screen,
pdf or ps files.  
}
\usage{
hardcopy(width=3.75, height=3.75, color=F, trellis=F,
             device=c("","pdf","ps"), path="", pointsize=c(8,4), horiz=F)
}
\arguments{
  \item{width}{ }
  \item{height}{ }
  \item{color}{TRUE if plot is not black on white only}
  \item{trellis}{TRUE if plot uses trellis graphics}
  \item{device}{screen "", pdf or ps}
  \item{path}{external file name}
  \item{pointsize}{}
  \item{horiz}{FALSE for landscape mode}
}
\value{
Graphical output to screen, pdf or ps file.
}
\author{J.H. Maindonald}
\seealso{\code{postscript}}

\keyword{IO}
