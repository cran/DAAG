\name{plotSampDist}
\alias{plotSampDist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot(s) of simulated sampling distributions}
\description{
  Plots are based on the output from \code{simulateSampDist()}.
  By default, both density plots and normal probability plots
  are given, for a sample from the specified population and
  for samples of the relevant size(s)
}
\usage{
plotSampDist(sampvalues, graph = c("density", "qq"), cex = 0.925,
             titletext = "Empirical sampling distributions of the",
             popsample=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sampvalues}{Object output from \code{simulateSampDist()}}
  \item{graph}{Either or both of \code{"density"} and \code{"qq"}}
  \item{cex}{Character size parameter, relative to default}
  \item{titletext}{Title for graph}
  \item{popsample}{If \code{TRUE} show distribution of random
  sample from population}
  \item{\dots}{Other graphics parameters}
}
\value{
Plots graph(s), as described above.
}
\references{Maindonald, J.H. and Braun, W.J. (3rd edn, 2010)
  \dQuote{Data Analysis and Graphics Using R}, Sections 3.3 and 3.4.}
\author{John Maindonald}
\seealso{See Also \code{help(simulateSampDist)}}
\examples{
## By default, sample from normal population
simAvs <- simulateSampDist()
par(pty="s")
plotSampDist(simAvs)
## Sample from empirical distribution
simAvs <- simulateSampDist(rpop=rivers)
plotSampDist(simAvs)

## The function is currently defined as
function(sampvalues, graph=c("density", "qq"), cex=0.925,
           titletext="Empirical sampling distributions of the",
           popsample=TRUE, ...){
    if(length(graph)==2)oldpar <- par(mfrow=c(1,2), mar=c(3.1,4.1,1.6,0.6),
               mgp=c(2.5, 0.75, 0), oma=c(0,0,1.5,0), cex=cex)
    values <- sampvalues$values
    numINsamp <- sampvalues$numINsamp
    funtxt <- sampvalues$FUN
    nDists <- length(numINsamp)+1
    nfirst <- 2
    legitems <- paste("Size", numINsamp)
    if(popsample){nfirst <- 1
                  legitems <- c("Size 1", legitems)
                }
    if(match("density", graph)){
      popdens <- density(values[,1], ...)
      avdens <- vector("list", length=nDists)
      maxht <- max(popdens$y)
      ## For each sample size specified in numINsamp, calculate mean
      ## (or other statistic specified by FUN) for numsamp samples
      for(j in nfirst:nDists){
        av <- values[, j]
        avdens[[j]] <- density(av, ...)
        maxht <- max(maxht, avdens[[j]]$y)
      }
    }
    if(length(graph)>0)
      for(graphtype in graph){
        if(graphtype=="density"){
          if(popsample)
          plot(popdens, ylim=c(0, 1.2*maxht), type="l", yaxs="i",
               main="")
          else plot(avdens[[2]], type="n", ylim=c(0, 1.2*maxht),
                    yaxs="i", main="")
          for(j in 2:nDists)lines(avdens[[j]], col=j)
          legend("topleft",
                 legend=legitems,
                 col=nfirst:nDists, lty=rep(1,nDists-nfirst+1), cex=cex)
        }
        if(graphtype=="qq"){
          if(popsample) qqnorm(values[,1], main="")
          else qqnorm(values[,2], type="n")
          for(j in 2:nDists){
            qqav <- qqnorm(values[, j], plot.it=FALSE)
            points(qqav, col=j, pch=j)
           }
            legend("topleft", legend=legitems,
                   col=nfirst:nDists, pch=nfirst:nDists, cex=cex)
       }
      }
    if(par()$oma[3]>0){
      outer <- TRUE
      line=0
    }  else
    {
      outer <- FALSE
      line <- 1.25
    }
    if(!is.null(titletext))
      mtext(side=3, line=line,
            paste(titletext, funtxt),
            cex=1.1, outer=outer)
    if(length(graph)>1)par(oldpar)
  }
}
\keyword{hplot}
\keyword{distribution}
