"cv.lm" <-
function(df = houseprices, m = 3, form.lm =formula(sale.price ~ area), dots=FALSE, seed=29)
{
    vars <- all.vars(form.lm)
    ynam <- vars[1]
    xnam <- vars[2]
    if(!is.null(seed))set.seed(seed)
    oldpar<-par(mar=par()$mar-c(1,0,2,0))
    on.exit(par(oldpar))
    coltypes <- c(2, 3, 6)
    ltypes <- 1:3
    ptypes <- 2:4
    options(digits=3)
    n <- dim(df)[1]
    rand <- sample(n)%%m+1
    xv <- df[, xnam]
    yv <- df[, ynam]
    plot(xv, yv, xlab = xnam, ylab = ynam, type = "n")
    xval <- pretty(xv, n = 20)
    df.lm <- lm(yv ~ xv, data=df)
    print(anova(df.lm))
    cat("\n")
    sumss<-0
    sumdf<-0
    par(lwd=2)
    for(i in sort(unique(rand))) {
        cat("\nfold", i, "\n")
        n.in <- (1:n)[rand != i]
        n.out <- (1:n)[rand == i]
        cat("Observations in test set:", n.out, "\n")
        ab <- lm(yv ~ xv, subset = n.in)$coef
        z <- xv[n.out]
        points(xv[n.out], yv[n.out], col=coltypes[i], pch = ptypes[i], cex = 1.25)
        if(dots)
            points(xv[n.out], yv[n.out], col = coltypes[i], pch = 16)
        pred <- ab[1] + ab[2] * z
        resid <- yv[n.out] - pred
        xy <- data.frame(rbind(z,pred, yv[n.out], resid
            ), row.names=c(xnam,"Predicted", ynam,"Residual"))
        yval <- ab[1] + ab[2] * xval
        lines(xval, yval, lwd = 2, col = coltypes[i], lty=ltypes[i])
        num <- length(n.out)
        print(xy,collab=rep("",num))
        ss <- sum(resid^2)
        sumss<-sumss+ss
        sumdf<-sumdf+num
        ms <- ss/num
        cat("\nSum of squares =", round(ss, 2), "   Mean square =", 
            round(ms, 2), "   n =", num, "\n")
    }
    print(c("Overall ms"=sumss/sumdf))
    topleft<-par()$usr[c(1,4)]
    par(lwd=1)
    legend(topleft[1],topleft[2],legend=paste("Fold",1:m),pch=ptypes,
        lty=ltypes,col=coltypes, cex=0.75)
    par(col = 1)
 }
