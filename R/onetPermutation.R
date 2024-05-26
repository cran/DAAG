
"onetPermutation" <-
  function (x = DAAG::pair65$heated - DAAG::pair65$ambient, nsim = 2000, 
            plotit = TRUE) 
  {
    n <- length(x)
    dbar <- mean(x)
    absx <- abs(x)
    z <- array(, nsim)
    for (i in 1:nsim) {
      mn <- sample(c(-1, 1), n, replace = TRUE)
      xbardash <- mean(mn * absx)
      z[i] <- xbardash
    }
    stat.dist <- abs(z)
    ngeq <- sum(stat.dist>=abs(dbar))
    if (plotit) {
      plot(density(z), xlab = "", main = "", yaxs = "i", cex.axis = 0.8, 
           bty = "L")
      abline(v = dbar)
      abline(v = -dbar, lty = 2)
      mtext(side = 3, line = 0.5, text = expression(bar(d)), 
            at = dbar)
      mtext(side = 3, line = 0.5, text = expression(-bar(d)), 
            at = -dbar)
    }
    pval <- ngeq/nsim
    ## Possible refinement; see ?statmod::permp
    pval0 <- statmod::permp(x=ngeq, nperm=nsim, total.nperm=2^n)
    print(signif(pval, 3))
    invisible()
  }
# See ?EnvStats::oneSamplePermutationTest
# Phipson B, Smyth GK (2010). Permutation P-values should never be zero: 
# calculating exact P-values when permutations are randomly drawn. 
# Statistical Applications in Genetics and Molecular Biology, Volume 9, 
# Issue 1, Article 39. doi:10.2202/1544-6115.1585, doi:10.48550/arXiv.1603.05766.

