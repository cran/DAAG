\name{cuckoohosts}
\alias{cuckoohosts}
\docType{data}
\title{Comparison of cuckoo eggs with host eggs}
\description{
  These data compare mean length, mean breadth, and egg color,
  between cuckoos and their hosts.
}
\usage{cuckoohosts}
\format{
  A data frame with 10 observations on the following 12 variables.
  \describe{
    \item{clength}{mean length of cuckoo eggs in given host's nest}
    \item{cl.sd}{standard deviation of cuckoo egg lengths}
    \item{cbreadth}{mean breadth of cuckoo eggs in given host's nest}
    \item{cb.sd}{standard deviation of cuckoo egg breadths}
    \item{cnum}{number of cuckoo eggs}
    \item{hlength}{length of host eggs}
    \item{hl.sd}{standard deviation of host egg lengths}
    \item{hbreadth}{breadth of host eggs}
    \item{hb.sd}{standard deviation of host egg breadths}
    \item{hnum}{number of host eggs}
    \item{match}{number of eggs where color matched}
    \item{nomatch}{number where color did not match}
  }
}
\details{
  Although from the same study that generated data in the data frame
  \code{cuckoos}, the data do not match precisely.  The cuckoo egg
  lengths and breadths are from the tables on page 168, the host egg
  lengths and breadths from Appendix IV on page 176, and the color
  match counts from the table on page 171.
}
\source{
Latter, O.H., 1902.
The egg of \emph{cuculus canorus}. an inquiry into the dimensions of
  the cuckoo's egg and the relation of the variations to the size of the eggs
  of the foster-parent, with notes on coloration, &c.
\emph{Biometrika}, 1:164--176.
}
\examples{
cuckoohosts
str(cuckoohosts)
plot(cuckoohosts)
with(cuckoohosts,
     plot(c(clength,hlength),c(cbreadth,hbreadth),col=rep(1:2,c(6,6))))
}
\keyword{datasets}
