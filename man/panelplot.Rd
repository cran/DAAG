\name{panelplot}
\alias{panelplot}
\title{Panel Plot}
\description{
Panel plots of various types.
}
\usage{
panelplot(data, panel=points, totrows=3, totcols=2, oma=rep(2.5, 4), par.strip.text=NULL)
}
\arguments{
  \item{data}{ A list consisting of elements, each of which
consists of x, y, xlim and ylim vectors}
  \item{panel}{The panel function to be plotted}
  \item{totrows}{The number of rows in the plot layout}
  \item{totcols}{The number of columns in the plot layout}
  \item{oma}{Outer margin area}
  \item{par.strip.text}{A data frame with column cex}
}
\author{J.H. Maindonald}

\examples{
     x1 <- x2 <- x3 <- (11:30)/5
     y1 <- x1 + rnorm(20)/2
     y2 <- 2 - 0.05 * x1 + 0.1 * ((x1 - 1.75))^4 + 1.25 * rnorm(20)
     r <- round(cor(x1, y2), 3)
     rho <- round(cor(rank(x1), rank(y2)), 3)
     y3 <- (x1 - 3.85)^2 + 0.015 + rnorm(20)/4
     theta <- ((2 * pi) * (1:20))/20
     x4 <- 10 + 4 * cos(theta)
     y4 <- 10 + 4 * sin(theta) + (0.5 * rnorm(20))
     r1 <- cor(x1, y1)
     xy <- data.frame(x = c(rep(x1, 3), x4), y = c(y1, y2, y3, y4), gp = rep(                1:4, rep(20, 4)))
     xy <- split(xy,xy$gp)
     xlimdf<-lapply(list(x1,x2,x3,x4),range)
     ylimdf<-lapply(list(y1,y2,y3,y4),range)
     xy <- lapply(1:4,function(i,u,v,w){u[[i]]$xlim<-v[[i]];
     u[[i]]$ylim<-w[[i]]; u[[i]]},u=xy,v=xlimdf,w=ylimdf)
 panelplot(xy,panel=panel.corr,totrows=2,totcols=2, oma=rep(1,4))
}
\keyword{models}
