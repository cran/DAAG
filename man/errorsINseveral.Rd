\name{errorsINseveral}
\Rdversion{1.1}
\alias{errorsINseveral}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Simulation of classical errors in x model, with multiple
  explanatory variables.
}
\description{
Simulates $y-$ and $x-$values for a classical \dQuote{errors in $x$}
linear regression model. One or more $x-$values are subject to random
measurement error, independently of the corresponding covariate
values that are measured without error.
}
\usage{
errorsINseveral(n = 1000, a0 = 2.5, beta = c(1.5, 0), mu = 12.5, SDyerr = 0.5, default.Vpar = list(SDx = 2, rho = -0.5, timesSDx = 1.5), V = with(default.Vpar, matrix(c(1, rho, rho, 1), ncol = 2) * SDx^2), xerrV = with(default.Vpar, matrix(c(1, 0, 0, 0), ncol = 2) * (SDx * timesSDx)^2), parset = NULL, print.summary = TRUE, plotit = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of observations
}
  \item{a0}{
Intercept in linear regression model
}
  \item{beta}{
    Regression coefficients.  If one coefficient only is given, this will
    be repeated as many times as necessary
}
  \item{mu}{
Vector of covariate means.
}
  \item{SDyerr}{
SD of $y$, conditional on the covariates measured without error
}
  \item{default.Vpar}{
Parameters for the default model with two explanatory variables,
}
  \item{V}{
    Variance-covariance matrix for the z's, measured without error.
    (These are generated from a multivariate normal distribution,
    mainly as a matter of convenience)
}
  \item{xerrV}{
Variance-covariance matrix for the added \dQuote{errors in x}
}
  \item{parset}{
Parameter list (theme) in a form suitable for supplying to
  \code{trellis.par.set()}.
}
  \item{print.summary}{
    If \code{TRUE}, print summary details of the regression results from the
    simulation.
}
  \item{plotit}{
    If \code{TRUE}, plot the fitted values for the model with covariates with
    error, against the fitted values for covariates without error.
}
}
\details{
  With default arguments, simulates a model in which two
  covariates are in contention, the first measured without error,
  and the second with coefficient 0 in the model that includes
  both covariates measured without error.
}
\value{
\item{ERRfree}{Data frame holding covariates without error, plus $y$}
\item{addedERR}{Data frame holding covariates with error, plus $y$}
}
\references{
Data Analysis and Graphics Using R, 3rd edn, Section 6.8.1
}
\author{
John Maindonald
}
\seealso{
  \code{\link{errorsINx}}
}
\examples{
library(lattice)
function(n=1000, a0=2.5, beta=c(1.5,0), mu=12.5, SDyerr=0.5, 
           default.Vpar=list(SDx=2, rho=-0.5, timesSDx=1.5),
           V=with(default.Vpar, matrix(c(1,rho,rho,1), ncol=2)*SDx^2),
           xerrV=with(default.Vpar, matrix(c(1,0,0,0), ncol=2)*(SDx*timesSDx)^2),
           parset=NULL, print.summary=TRUE, plotit=TRUE){
    m <- dim(V)[1]
    if(length(mu)==1)mu <- rep(mu,m)
    ow <- options(warn=-1)
    xxmat <- sweep(matrix(rnorm(m*n, 0, 1), ncol=m) \%*\% chol(V), 2, mu, "+")
    errxx <- matrix(rnorm(m*n, 0, 1), ncol=m) \%*\% chol(xerrV, pivot=TRUE)
    options(ow)
    dimnames(xxmat)[[2]] <- paste("z", 1:m, sep="")
    xxWITHerr <- xxmat+errxx
    xxWITHerr <- data.frame(xxWITHerr)
    names(xxWITHerr) <- paste("xWITHerr", 1:m, sep="")
    xxWITHerr[, "y"] <- a0 + xxmat \%*\% matrix(beta,ncol=1) + rnorm(n, sd=SDyerr)
    err.lm <- lm(y ~ ., data=xxWITHerr)
    xx <- data.frame(xxmat)
    names(xx) <- paste("z", 1:m, sep="")
    xx$y <- xxWITHerr$y
    xx.lm <- lm(y ~ ., data=xx)
    B <- coef(err.lm)
    b <- coef(xx.lm)
    SE <- summary(err.lm)$coef[,2]
    se <- summary(xx.lm)$coef[,2]
    if(print.summary){
      beta0 <- c(mean(xx$y)-sum(beta*apply(xx[,1:m],2,mean)), beta)
      tab <- rbind(beta0, b, B)
      dimnames(tab) <- list(c("Values for simulation",
                              "Estimates: no error in x1",
                              "LS Estimates: error in x1"),
                            c("Intercept", paste("b", 1:m, sep="")))
      tabSE <- rbind(rep(NA,m+1),se,SE)
      rownames(tabSE) <- rownames(tab)
      colnames(tabSE) <- c("SE(Int)", paste("SE(", colnames(tab)[-1],")", sep=""))
      tab <- cbind(tab,tabSE)
      print(round(tab,3))
    }
    if(m==2 & print.summary){
      tau <- default.Vpar$timesSDx
      s1 <- sqrt(V[1,1])
      s2 <- sqrt(V[2,2])
      rho <- default.Vpar$rho
      s12 <- s1*sqrt(1-rho^2)
      lambda <- (1-rho^2)/(1-rho^2+tau^2)
      gam12 <- rho*sqrt(V[1,1]/V[2,2])
      expB2 <- beta[2]+beta[1]*(1-lambda)*gam12
      print(c("Theoretical attenuation of b1" = lambda, "Theoretical b2" = expB2))
    }
    if(is.null(parset))parset <- simpleTheme(col=c("gray40","gray40"),
                                             col.line=c("black","black"))
    if(plotit){
      library(lattice)
      zhat <- fitted(xx.lm)
      xhat <- fitted(err.lm)
      plt <- xyplot(xhat ~ zhat, aspect=1, scales=list(tck=0.5),
                    panel=function(x,y,...){
                      panel.xyplot(x,y,type="p",...)
                      panel.abline(lm(y ~ x), lty=2)
                      panel.abline(0,1)
                    },
                    xlab="Fitted values; regress on exact z",
                    ylab="Fitted values; regress on x = xWITHerr",
                    key=list(space="top", columns=2,
                      text=list(lab=c("Line y=x", "Regression fit to points")),
                      lines=list(lty=1:2)),
                    par.settings=parset
                    )
      print(plt)}
    invisible(list(ERRfree=xx, addedERR=xxWITHerr))
  }
}
\keyword{datagen}

