\name{hardcopy}
\alias{hardcopy}
\title{Graphical Output for Hardcopy}
\description{
This function streamlines graphical output to the screen, pdf or ps
files. File names for hard copy devices can be generated
automatically from function names of the form \code{g3.2} or
\code{fig3.2} (the choice of alphabetic characters prior to
\code{3.2} is immaterial).
}
\usage{
hardcopy(width = 3.75, height = 3.75, color = FALSE, trellis = FALSE,
                 device = c("", "pdf", "ps"), path = getwd(), file =
                 NULL, format = c("nn-nn", "name"), split = "\\\\.",
                 pointsize = c(8, 4), fonts=NULL, horiz = FALSE, ...)
}
\arguments{
  \item{width}{width of plot in inches (sic!)}
  \item{height}{height of plot in inches (sic!)}
  \item{color}{(lattice plots only) TRUE if plot is not black on white only}
  \item{trellis}{TRUE if plot uses trellis graphics}
  \item{device}{screen "", pdf or ps}
  \item{path}{external path name}
  \item{file}{name of file to hold output, else \code{NULL}}
  \item{format}{Alternatives are \code{"nn-nn"} and \code{"name"}.}
  \item{split}{character on which to split function name (file=NULL)}
  \item{pointsize}{Pointsize. For trellis devices a vector of length
    2 giving font sizes for text and for points respectively}
  \item{fonts}{For postscript devices, specify families that will be
    used in addition to the intial device}
  \item{horiz}{FALSE for landscape mode; applies only to postscript
    files}
  \item{...}{Other arguments for passing to the \code{pdf} or
    \code{postscript}}
}

\details{If a file name (\code{file}, without extension) is not
  supplied, the \code{format} argument determines how the name is
  constructed.  With \code{format="name"}, the function name is used.
  With \code{format="nn-nn"} and \code{dotsplit} unchanged from the
  default, a function name of the form g3.1 leads to the name
  \code{03-01}.  Here \code{g} can be replaced by any other non-numeric
  characters; the result is the same. The relevant extension is in any
  case added.
  }
\value{
  Graphical output to screen, pdf or ps file. 
}
\author{J.H. Maindonald}
\seealso{\code{\link{postscript}}}

\keyword{IO}
