qreference <-
function (test = NULL, m = 30, nrep = 6, 
          pch = c(16,2), distribution = function(x) qnorm(x, 
            mean = ifelse(is.null(test), 0, mean(test)), 
            sd = ifelse(is.null(test), 1, sd(test))), 
          seed = NULL, nrows = NULL, cex.strip = 0.75, 
          xlab = NULL, ylab = NULL) 
{
  if (!is.null(seed)) 
    set.seed(seed)
  if (!is.null(test)) {
    testnam <- deparse(substitute(test))
    m <- length(test)
    av <- mean(test)
    
    sdev <- sd(test)
    fac <- factor(c(rep(testnam, m), paste("reference", rep(1:(nrep - 
                                                                 1), rep(m, (nrep - 1))))))
    fac <- relevel(fac, ref = testnam)
  }
  id <- NULL
  if (is.null(nrows)) 
    nrows <- floor(sqrt(nrep))
  ncols <- ceiling(nrep/nrows)
  if (is.null(test)) {
    if(is.null(xlab))xlab <- ""
    if(is.null(ylab))ylab <- ""  
    xy <- data.frame(y = distribution(runif(m * nrep)), 
                     fac = factor(rep(1:nrep, rep(m, nrep))), 
                     id = factor(rep(1, m * nrep)))
    qq <- lattice::qqmath(~y | fac, data = xy, par.strip.text = list(cex = 0), 
                          distribution = distribution, layout = c(ncols, nrows), 
                          xlab = xlab, ylab = ylab, aspect = 1, pch=pch[1])
  }
  else {
    if (length(test) > 0) {
      yy <- quantile(test, c(0.25, 0.75))
      xx <- distribution(c(0.25, 0.75))
      r <- diff(yy)/diff(xx)
    }
    xy <- data.frame(y = c(test, 
                           distribution(runif(m * (nrep - 1)))), 
                     fac = fac, id = factor(rep(1:2, c(m, m * (nrep - 1)))))
    if (is.null(xlab)) 
      xlab <- paste("Quantiles of", deparse(substitute(distribution)))
    if (is.null(ylab)) 
      ylab <- ""
    qq <- lattice::qqmath(~y | fac, data = xy, 
                          layout = c(ncols, nrows), groups = id, aspect = 1, 
                          xlab = xlab, ylab = "", distribution = distribution, 
                          par.strip.text = list(cex = cex.strip), pch=pch)
  }
  qq
}
