\name{errorsINx}
\alias{errorsINx}
\title{Simulate data for straight line regression, with "errors in x".}
\description{
  Simulates $y-$ and $x-$values for the straight line regression model,
  but with $x-$values subject to random measurement error, following
  the classical \dQuote{errors in x} model. Optionally, the x-values
  can be split into two groups, with one group shifted relative to the other
}
\usage{errorsINx(mu = 12.5, n = 200, a = 15, b = 1.5, SDx=2, SDyerr = 1.5,
           timesSDx=(1:5)/2.5, gpfactor=if(missing(gpdiff))FALSE else TRUE,
           gpdiff=if(gpfactor) 1.5 else 0, layout=NULL,
           parset = simpleTheme(alpha = 0.75, col = c("black","gray45"),
             col.line = c("black","gray45"), lwd=c(1,1.5), pch=c(1,2),
           lty=c(1,2)), print.summary=TRUE, plotit=TRUE)
}
\arguments{
  \item{mu}{Mean of $z$}
  \item{n}{Number of points}
  \item{a}{Intercept in model where $z$ is measured without error}
  \item{b}{Slope in model where $z$ is measured without error}
  \item{SDx}{SD of $z$-values, measured without error}
  \item{SDyerr}{SD of error term in \code{y} where $z$ is measured without error}
  \item{timesSDx}{SD of measurement error is \code{timesSDx}, as a
    multiple of \code{SDx}}
  \item{gpfactor}{Should x-values be split into two groups, with one
  shifted relative to the other?}
  \item{gpdiff}{Amount of shift of one group of z-values relative to the other}
  \item{layout}{Layout for lattice graph, if requested}  
  \item{parset}{Parameters to be supplied to the lattice plot, if any}
  \item{print.summary}{Print summary information on fits?}  
  \item{plotit}{logical: plot the data?}
}
\details{
  The argument \code{timesSDx} can be a numeric vector.
  One set of $x$-values that are contaminated with measurement error
  is simulated for each element of \code{timesSDx}.
}
\value{
  A matrix, with \code{length(timesSDx)+2} columns.  Values of $z$ are
  in the first column. There is one further column (x with error) for
  each element of \code{timesSDx}, followed by a column for $y$.
  If there is a grouping variable, a further column identifies the
  groups.
}
\references{Data Analysis and Graphics Using R, 2nd edn, Section 6.8.1}
\author{John Maindonald}
\examples{
library(lattice)
errorsINx()
## The function is currently defined as
errorsINx <-
  function(mu = 12.5, n = 200, a = 15, b = 1.5, SDx=2, SDyerr = 1.5,
           timesSDx=(1:5)/2.5, gpfactor=if(missing(gpdiff))FALSE else TRUE,
           gpdiff=if(gpfactor) 1.5 else 0, layout=NULL,
           parset = simpleTheme(alpha = 0.75, col = c("black","gray45"),
             col.line = c("black","gray45"), lwd=c(1,1.5), pch=c(1,2),
           lty=c(1,2)), print.summary=TRUE, plotit=TRUE){
    m <- length(timesSDx)+1
    if(!gpfactor)
        mat <- matrix(0, nrow=n, ncol=m+1) else
        mat <- matrix(0, nrow=n, ncol=m+2)    
    x0 <- rnorm(n, mu, SDx)
    mat[,1] <- x0
    sx <- sd(x0)
    if(gpdiff!=0){
      gps <- factor(sample(1:2, n, replace=TRUE), labels=c("ctl", "trt"))
      x0[gps=="trt"] <- x0[gps=="trt"] + gpdiff
      mat[, m+2] <- gps
    } else gps <- NULL
    y <- a + b*x0+rnorm(n,0,SDyerr)
    if(print.summary){
      sumtab <- matrix(0, nrow=m, ncol=2+as.numeric(gpdiff>0))
      sumtab[1,1:2] <- coef(lm(y ~ x0))
      if(!is.null(gps))sumtab[1,] <- coef(lm(y ~ gps+x0))
      if(is.null(gps))form <- formula(y ~ xWITHerror) else
      form <- formula(y ~ gps+xWITHerror)
      dimnames(sumtab) <- list(paste(c("", paste(timesSDx)),
                                     c("No error in x", rep("sx",m-1)), sep=""),
                               if(is.null(gps))c("Intercept", "Slope")
                               else c("Intercept:ctl", "Offset:trt", "Slope"))
    }

    mat[,1] <- x0
    mat[,m+1] <- y
    k <- 1
    for(tau in timesSDx){
      k <- k+1
      xWITHerror <- x0+rnorm(n, 0, sx*tau)
      mat[, k] <- xWITHerror
      if(print.summary)sumtab[k,] <- coef(lm(form))
    }
    if(print.summary)print(sumtab)
    nam <- c(paste("x", c(0, timesSDx), sep=""), "y", if(gpfactor)"gp" else NULL)
    dimnames(mat)[[2]] <- nam
    if(plotit){
    library(lattice)
    matdf <- data.frame(x <- as(mat[,1:m], "vector"), y=rep(mat[,m+1], m), 
                        tau=factor(rep(c(0, timesSDx), rep(n,m))))
    names(matdf)[1] <- "x"
    if(!is.null(gps))matdf$gps <- rep(gps, m)      
      lab <- c(list("0 (No error in x)"),
               lapply(timesSDx, function(x)bquote(.(x))))
      lab <- as.expression(lab)
      if(gpdiff==0){
        print(xyplot(y ~ x | tau, data=matdf, outer=TRUE,
                     aspect=1,  between=list(x=0.25, y=0.25),
                     xlab="Explanatory variable (z or x=zWITHerr)",
                     par.settings=parset, layout=layout,
                     strip=strip.custom(strip.names=TRUE, factor.levels=lab,
                       var.name=expression(tau), sep=expression(" = ")),
                     par.strip.text=list(cex=0.925), 
                     panel=function(x,y,...){
                       panel.xyplot(x,y,type=c("p","r"), ...)
                       panel.abline(a=a, b=b, lty=2)
                     },
                     scales=list(x=list(relation="free"), tck=0.5)))
      } else
      print(xyplot(y ~ x | tau, groups=gps, data=matdf, outer=TRUE,
                   par.settings=parset,
                   aspect=1,  between=list(x=0.25, y=0.25),
                   strip=strip.custom(strip.names=TRUE, factor.levels=lab,
                     var.name=expression(tau), sep=expression(" = ")),
                   par.strip.text=list(cex=0.925),  layout=layout,
                   panel=function(x,y,...){
                     panel.superpose(x,y,type="p",...)
                     subs <- list(...)$subscripts
                     fac <- list(...)$groups[subs]
                     gps.lm <- lm(y~fac+x)
                     sed <- summary(gps.lm)$coef[2,2]
                     ylim <- range(y)
                     xlim <- range(x)
                     mid <- c(xlim[1]+diff(xlim)*0.01, ylim[2]-diff(ylim)*0.01-sed/2)
                     panel.arrows(mid[1], mid[2]-sed/2, mid[1], mid[2]+sed/2,
                                  angle=90, ends="both", length=0.01)
                     panel.text(mid[1], mid[2], "SED", pos=4, cex=0.75)
                     hat <- fitted(gps.lm)
                     panel.superpose(x,y=hat,type="r",...)                     
                   },
                   auto.key=list(columns=2, lines=TRUE),
                   xlab="Explanatory variable (z or x=zWITHerr)",                
                   scales=list(x=list(relation="free"), tck=0.5)))
    }
    invisible(mat)
  }
}
\keyword{datagen}
