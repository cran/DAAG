"qreference" <-
function(test=NULL, mu = 10, sigma = 1, m = 50, nrep = 5, 
             seed=NULL, nrows=NULL, cex.points=0.65, cex.strip=0.75)
{
    library(lattice)
    if(!is.null(seed))set.seed(seed)
    if(!is.null(test)){
        testnam <- deparse(substitute(test))
        m <- length(test);
        av <- mean(test); sdev <- sd(test)
        fac <- factor(c(rep(testnam, m),
                        paste("reference", rep(1:(nrep-1), rep(m, (nrep-1))))))
        fac <- relevel(fac, ref=testnam)}
    if(is.null(nrows)) nrows <- floor(sqrt(nrep))
    ncols <- ceiling(nrep/nrows)
    if(is.null(test)){
        xy <- data.frame(y = rnorm(m*nrep, mu, sigma),
                         fac=factor(rep(1:nrep, rep(m, nrep))))
        qq <- qqmath(~y|fac, data=xy, par.strip.text=list(cex=0),
                     layout=c(ncols,nrows), xlab="",ylab="", aspect=1,
                     cex=cex.points)}
    else{
        xy <- data.frame(y = c(test, rnorm(m*(nrep-1), av, sdev)), fac=fac)
        qq <- qqmath(~y|fac, data=xy, layout=c(ncols,nrows), aspect=1,
                     xlab="",ylab="", cex=cex.points, pch=16,
                     par.strip.text=list(cex=cex.strip))}
    
    print(qq)
}
