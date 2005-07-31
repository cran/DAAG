\name{powerplot}
\alias{powerplot}
\title{Plot of Power Functions}
\description{
This function plots powers of a variable on the interval [0,10].
}
\usage{
powerplot(expr="x^2", xlab="x", ylab="y")
}
\arguments{
  \item{expr}{Functional form to be plotted}
  \item{xlab}{x-axis label}
  \item{ylab}{y-axis label}
}
\value{
A plot of the given expression on the interval [0,10]. 
}
\author{J.H. Maindonald}
\notes{Other expressions such as "sin(x)" and "cos(x)", etc.
could also be plotted with this function, but results are
not guaranteed.
}
\examples{
   oldpar <- par(mfrow = c(2, 3), mar = par()$mar - c(
        1, 1, 1.0, 1),  mgp = c(1.5, 0.5, 0),  oma=c(0,1,0,1))
#    on.exit(par(oldpar))
    powerplot(expr="sqrt(x)", xlab="")
    powerplot(expr="x^0.25", xlab="", ylab="")
    powerplot(expr="log(x)", xlab="", ylab="")
    powerplot(expr="x^2")
    powerplot(expr="x^4", ylab="")  
    powerplot(expr="exp(x)", ylab="")
par(oldpar)}
\keyword{models}
