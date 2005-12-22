\name{datafile}
\alias{datafile}
\title{Create an ASCII data file}
\description{
Invoking this function creates one of four data files used
in Chapters 1 and 14 of DAAGUR.
}
\usage{
datafile(file="fuel")
}
\arguments{
    \item{file}{ character; "fuel", for fuel.txt;
 "fuel.csv", for fuel.csv;
 "oneBadRow", for oneBadRow.txt;
 "scan-demo", for scan-demo.txt.}
}
\value{
One of four ASCII files is produced, and output to the current
working directory.
}
\author{J.H. Maindonald}

\keyword{models}
