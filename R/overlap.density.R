"overlap.density" <-
function(x0, x1, frac=c(.05, 20), plotit=TRUE){
n0 <- length(x0)
n1 <- length(x1)
d0 <- density(x0)
d1 <- density(x1)
f0 <- d0$y*n0
f1 <- d1$y*n1
xlim <- range(c(d0$x,d1$x),na.rm=TRUE)
ylim <- range(c(f0,f1))
ylim[2] <- ylim[2] + 0.1 * diff(ylim)
if(plotit){
plot(d1$x, f1, xlim=xlim, xlab="Score", xaxt="n", yaxs="i", ylim=ylim,
    ylab="Density x total frequency", main="", col=2, type="l", lty=1,lwd=2, bty="n")
lines(d0$x, f0, lwd=2, lty=2)
mtext(side=3,line=.75,"A",adj=0)
xpos <- par()$usr[1]
ypos <- par()$usr[4]
legend(xpos, ypos,lty=c(2,1), col=c("black","red"), cex=c(.8,.8),
    legend=c("Control","Treatment"),lwd=c(1,2),bty="n")
}
# We now need densities that are evaluated at the same x-values
d0 <- density(x0, from=xlim[1], to=xlim[2])
d1 <- density(x1, from=xlim[1], to=xlim[2])
x01 <- d0$x
f0 <- d0$y * n0
f1 <- d1$y * n1
eps <- .Machine$double.eps
f0[f0<eps] <- eps
f1[f1<eps] <- eps
xlim[1] <- min(x01[f1/f0 < frac[2] & f0/f1<frac[2]])
xlim[2] <- max(x01[f1/f0 > frac[1] & f0/f1>frac[1]])
if(plotit)axis(1, at=xlim)
invisible(xlim)
}
