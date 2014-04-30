\name{align2D}
\alias{align2D}
\title{
Function to align points from ordination with known locations
}
\description{
  Find the linear transformation which, applied to one set of points
  in the ($x$, $y$) plane, gives the best match in a least squares
  sense to a second set of points.
}
\usage{
align2D(lat, long, x1, x2, wts=NULL)
}
\arguments{
  \item{lat}{
Latitude or other co-ordinate of point to align to
}
  \item{long}{
Longitude or other co-ordinate of point to align to
}
  \item{x1}{
First coordinate of point to align
}
  \item{x2}{
First coordinate of point to align
}
  \item{wts}{
If non-NULL, specifies weights for the points.
}
}
\details{
  Achieves the best match, in a least squares sense, between an ordination
  and known locations in two-dimensionaL space.}
\value{
\item{fitlat}{Fitted values of \code{lat}}
\item{fitlong}{Fitted values of \code{long}}
\item{lat}{Input values of \code{lat}}
\item{long}{Input values of \code{long}}
}
\author{
John H Maindonald
}
\note{
  An ordination that is designed to reproduce distances between
  points is specified only to within an arbitrary rotation about
  the centroid.  What linear transformation of the points ($x1$, $x2$)
  given by the ordination gives the best match to the known
  co-ordinates?
}
\examples{
if(require(DAAG)&require(oz)){
aupts <- cmdscale(audists)
xy <- align2D(lat = aulatlong$latitude, long = aulatlong$longitude,
              x1 = aupts[, 1], x2 = aupts[, 2], wts = NULL)
oz()
fitcoords <- align2D(lat=aulatlong$latitude,
                      long=aulatlong$longitude,
                      x1=aupts[,1], x2 = aupts[,2],
                      wts=NULL)
x <-with(fitcoords,
         as.vector(rbind(lat, fitlat, rep(NA,length(lat)))))
y <-with(fitcoords,
         as.vector(rbind(long, fitlong, rep(NA,length(long)))))
points(aulatlong, col="red", pch=16, cex=1.5)
lines(x, y, col="gray40", lwd=3)
}

## The function is currently defined as
function(lat, long, x1, x2, wts=NULL){
    ## Get best fit in space of (latitude, longitude)
    if(is.null(wts))wts <- rep(1,length(x1))
    fitlat <- predict(lm(lat ~ x1+x2, weights=wts))
    fitlong <- predict(lm(long ~ x1+x2, weights=wts))
    list(fitlat = fitlat, fitlong=fitlong, lat=lat, long=long)
}
}
\keyword{dplot}
\keyword{algebra}% __ONLY ONE__ keyword per line
