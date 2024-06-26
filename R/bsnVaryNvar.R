bsnVaryNvar <-
function (m = 100, nvar = nvmax:50, nvmax = 3, method = "exhaustive",
          intercept = TRUE, plotit = TRUE, xlab = "# of variables from which to select",
          ylab = "p-values for t-statistics", main = paste("Select 'best'",
                                                           nvmax, "variables"), details = FALSE, really.big = TRUE,
          smooth = TRUE, ...)
{
  if (nvar[1] < nvmax)
    stop(paste("Initial value of 'num' must be at least",
               nvmax))
  leaps.out <- try(requireNamespace("leaps"), silent = TRUE)
  if (!is.logical(leaps.out) | (leaps.out == FALSE)) {
    print("Error: package leaps is not installed properly")
    return()
  }
  qgam.out <- try(requireNamespace("qgam"), silent = TRUE)
  best <- matrix(0, nrow = length(nvar), ncol = nvmax)
  if (details) {
    bestCoef <- bestSE <- best
  }
  k <- 0
  for (i in nvar) {
    k <- k + 1
    obj <- bestsetNoise(m = 100, n = i, nvmax = nvmax, intercept = intercept,
                        print.summary = FALSE, method = method, really.big = really.big)
    if (intercept)
      bmat <- coef(summary(obj$best))[2:(nvmax + 1), ]
    else bmat <- coef(summary(obj$best))[1:nvmax, ]
    best[k, ] <- bmat[, 4]
    if (details) {
      bestCoef[k, ] <- bmat[, 1]
      bestSE[k, ] <- bmat[, 2]
    }
  }
  if (plotit) {
    v <- as.vector(best)
    x <- rep(nvar, nvmax)
    clogy <- -log(-log(v))
    xlim <- c(0, max(nvar) + 1)
    plot(x, clogy, yaxt = "n", xlab = xlab, ylab = ylab,
         col = "gray30", xlim = xlim, xaxs = "i", xaxt = "n", ...)
    pval <- c(0.001, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95)
    g <- -log(-log(pval))
    if (smooth)
      if (qgam.out) {
          mod.ns <- qgam::qgam(clogy ~ s(logx),
                               data=data.frame(clogy=clogy, logx=log(x)),
                               qu = 0.5)
        hat <- predict(mod.ns)[1:length(nvar)]
        lines(nvar, hat, col = "gray40", lwd = 1.5)
      }
    else {
      print("Error: package qgam is not installed properly,")
      print("or not installed. Unable to fit smooth curve")
    }
    axis(1, at = nvar, tck = -0.012, labels = FALSE)
    axis(1, at = pretty(nvar), tck = -0.02)
    axis(2, at = g, labels = paste(pval), pos = 0, tck = -0.02, las=1)
    title(main = main, cex.main = 1.1, line=0.25)
  }
  if (details)
    list(coef = bestCoef, SE = bestSE, pval = best)
  else invisible(best)
}
