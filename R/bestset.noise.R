"bestset.noise" <-
function(m = 100, n = 40)
{
leaps.out <- try(require(leaps), silent=TRUE)
if ((is.logical(leaps.out)==TRUE)&(leaps.out==TRUE)){
    y <- rnorm(m)
    xx <- matrix(rnorm(m * n), ncol = n)
dimnames(xx)<-list(NULL, paste("V",1:n,sep=""))
    u <- regsubsets(xx, y, method = "exhaustive", nvmax = 3, nbest = 1)
    best3 <- summary(u)$which[3,-1]
    u1 <- lm(y ~ xx[,best3])
    print(summary(u1, corr = FALSE))
    invisible(u1)
    }
    else {
    print("Error: package leaps is not installed properly")
}
}
