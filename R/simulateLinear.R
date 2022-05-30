simulateLinear <-
function (sd = 2, npoints = 5, nrep = 4, nsets = 200, graphtype = "xy",
    seed = 21, ...)
{
    if (!is.null(seed))
        set.seed(seed)
    nval <- npoints * nrep
    tmp <- data.frame(x = rep(1:npoints, rep(nrep, npoints)))
    p.aov <- array(0, nsets)
    p.slope <- array(0, nsets)
    for (i in 1:nsets) {
        tmp$y <- 100 + 0.8 * tmp$x + rnorm(nval, 0, sd)
        u <- lm(y ~ factor(x), data = tmp)
        z <- summary.aov(u)
        p.aov[i] <- z[[1]][1, "Pr(>F)"]
        u <- lm(y ~ x, data = tmp)
        z1 <- summary(u)
        p.slope[i] <- z1$coef[2, 4]
    }
    logit <- function(p) log(p/(1 - p))
    x <- logit(p.aov)
    y <- logit(p.slope)
    pval <- c(0.001, 0.01, 0.1, 0.5, 0.9)
    xpos <- logit(pval)
    frac <- sum(y>x)/nsets
    keytxt <- paste("Fraction with y>x is", frac)
    xlim <- range(c(x, y), na.rm = TRUE)
    if (graphtype == "xy") {
        xtxt <- "p-val: Qualitative aov comparison"
        ytxt <- "p-val: Test for linear trend"
        gph <- lattice::xyplot(y~x, xlim = xlim, ylim = xlim,
                               xlab = xtxt, ylab = ytxt,
                     cex = 0.75, main = "",
                     legend=list(inside=list(fun=grid::textGrob,
                                             corner=c(0,1), x=0.02, y=0.98,
                                          args=list(label=keytxt))),
                     scales=list(x=list(at=xpos, labels=paste(pval)),
                         x=list(at=xpos, labels=paste(pval))),
                               panel=function(x,y,...){
                                   lattice::panel.points(x,y,...)
                                   lattice::panel.abline(0,1)
                               },
                     ...)
    }
    else if (graphtype == "density") {
        gps <- factor(rep(1:2, rep(nsets,2)), labels=c("aov", "linear"))
        gph <- lattice::densityplot(c(x,y), groups=gps,
                      xlab = "Density curves - 2 sets of p-values",
                      auto.key=list(columns=2),
                      scales=list(x=list(at=xpos, labels=paste(pval)),
                                  x=list(at=xpos, labels=paste(pval))),
                      ...)
    }
    else if (graphtype == "density-diff") {
        diffxy <- x-y
        gph <- lattice::densityplot(diffxy, main = "",
                                    xlab = "Difference in p-values, logit scale")
    }
    gph
}
