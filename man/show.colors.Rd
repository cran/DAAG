\name{show.colors}
\alias{show.colors}
\title{Show R's Colors}
\description{
This function displays the built-in colors.
}
\usage{
show.colors(type=c("singles", "shades", "grayshades"), order.cols=TRUE)
}
\arguments{
  \item{type}{type of display - single, multiple or gray shades}
  \item{order.cols}{ Arrange colors in order }
}
\value{
A plot of colors for which there is a single shade (type = "single"),
multiple shades (type = "multiple"), or gray shades (type = "gray")
}
\author{J.H. Maindonald}

\examples{
require(MASS)
show.colors()
}
\keyword{models}
