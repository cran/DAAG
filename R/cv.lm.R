cv.lm <-
function (df = houseprices, form.lm = formula(sale.price ~ area),
               m=3, dots = FALSE, seed = 29, plotit=TRUE, printit=TRUE)
{
     if(class(form.lm)=="call"|class(form.lm)=="formula")
         vars <- all.vars(form.lm)
     else if(class(form.lm)=="lm") vars <- all.vars(form.lm$call)
     else stop("form.lm must be formula or call or lm object")
     ynam <- vars[1]
     xnam <- vars[2]
     if (!is.null(seed))
         set.seed(seed)
     oldpar <- par(mar = par()$mar - c(1, 0, 2, 0))
     on.exit(par(oldpar))
     options(digits = 3)
     n <- dim(df)[1]
     rand <- sample(n)%%m + 1
     xv <- df[, xnam]
     yv <- df[, ynam]
     if(plotit){
         coltypes <- palette()[c(2, 3, 6, 1, 4:5,7)]
         if(m>7)coltypes <- c(coltypes,rainbow(m-7))
         ltypes <- 1:m
         ptypes <- 2:(m+1)
         plot(xv, yv, xlab = xnam, ylab = ynam, type = "n")
     }
     xval <- pretty(xv, n = 20)
     df.lm <- lm(yv ~ xv, data = df)
     if(printit){
         print(anova(df.lm))
         cat("\n")
     }
     sumss <- 0
     sumdf <- 0
     par(lwd = 2)
     for (i in sort(unique(rand))) {
         n.in <- (1:n)[rand != i]
         n.out <- (1:n)[rand == i]
         if(printit){
             cat("\nfold", i, "\n")
             cat("Observations in test set:", n.out, "\n")
         }
         ab <- lm(yv ~ xv, subset = n.in)$coef
         z <- xv[n.out]
         pred <- ab[1] + ab[2] * z
         resid <- yv[n.out] - pred
         xy <- data.frame(rbind(z, pred, yv[n.out], resid),
                          row.names = c(xnam, "Predicted", ynam, 
"Residual"))
         yval <- ab[1] + ab[2] * xval
         if(plotit){
             points(xv[n.out], yv[n.out], col = coltypes[i], pch = 
ptypes[i],
                cex = 1.25)
             if (dots)
                 points(xv[n.out], yv[n.out], col = coltypes[i], pch = 
16)
             lines(xval, yval, lwd = 2, col = coltypes[i], lty = 
ltypes[i])
     }
         num <- length(n.out)
         if(printit) print(xy, collab = rep("", num))
         ss <- sum(resid^2)
         sumss <- sumss + ss
         sumdf <- sumdf + num
         ms <- ss/num
         if(printit)
         cat("\nSum of squares =", round(ss, 2), "   Mean square =",
             round(ms, 2), "   n =", num, "\n")
     }
     if(printit) print(c("Overall ms" = sumss/sumdf))
     if(plotit){
         topleft <- par()$usr[c(1, 4)]
         par(lwd = 1)
         legend(topleft[1], topleft[2], legend = paste("Fold", 1:m),
                pch = ptypes, lty = ltypes, col = coltypes, cex = 0.75)
         par(col = 1)
     }
     invisible(c(ss=sumss, df=sumdf))
}

