\name{DAAGxdb}
\alias{DAAGxdb}
\docType{data}
\title{List, each of whose elements hold rows of a file, in character format}
\description{
  This is the default database for use with the function
  \code{datafile}, which uses elements of this list to place files
  in the working directory. 
}
\usage{data(DAAGxdb)}
\format{  
  Successive elements in this list hold character vectors from which the
  corresponding files can be generated.  The names of the list elements
  are fuel, fuel.csv, oneBadRow, scan-demo, molclock1, molclock2, and
  travelbooks. 
}
\details{
  The files fuel.txt and fuel.csv are used in Chapter 1 of DAAGUR, while
  the files oneBadRow.txt and scan-demo.txt are used in Chapter 14 of
  DAAGUR.}
\references{
  Maindonald, J.H. and Braun, W.J. 2007. Data Analysis and Graphics
  Using R: An Example-Based Approach.  2nd edn, Cambridge University
  Press (DAAGUR).
  }
\examples{
data(DAAGxdb)
names(DAAGxdb)
}
\keyword{datasets}
