`CVlm` <-
function (df = houseprices, form.lm = formula(sale.price ~ area), 
            m = 3, dots = FALSE, seed = 29, plotit = TRUE, printit = TRUE) 
{
  if (!(class(form.lm) %in% c("call", "formula", "lm")))
     stop("form.lm must be formula or call or lm object")   
  if (class(form.lm) == "lm")form.lm <- formula(form.lm)
  if (class(form.lm) == "call")form.lm <- formula(form.lm)
  vars <- all.vars(form.lm)
  ynam <- vars[1]
  if(length(vars)==2)multilr <- FALSE else multilr <- TRUE
  if (!is.null(seed)) 
    set.seed(seed)
  oldpar <- par(mar = par()$mar - c(1, 0, 2, 0))
  on.exit(par(oldpar))
  options(digits = 3)
  n <- dim(df)[1]
  ## Use to reproduce the results in DAAGUR
  if(seed==29 & m==3) rand <- sample(rep(c(2,3,1), length.out=n)) else
  rand <- sample(rep(1:m, length.out=n))
  yv <- df[, ynam]
  df.lm <- lm(form.lm, data = df)
  if (printit) {
    print(anova(df.lm))
    cat("\n")
  }
  if (plotit) {
      coltypes <- palette()[c(2, 3, 6, 1, 4:5, 7)]
      if (m > 7) coltypes <- c(coltypes, rainbow(m - 7))
      ltypes <- 1:m
     ptypes <- 2:(m + 1)
      if (!multilr) {
        xnam <- vars[2]
        xv <- df[, xnam]
      }
      else{
        xv <- predict(df.lm)
        allpred <- xv
        xnam <- "Predicted (all data)"
      }      
     plot(xv, yv, xlab = xnam, ylab = ynam, type = "n")
     topleft <- par()$usr[c(1, 4)]
     par(lwd = 1)
     legend(topleft[1], topleft[2], legend = paste("Fold", 1:m),
            pch = ptypes, lty = ltypes, col = coltypes, cex = 0.75)
  }  
  sumss <- 0
  sumdf <- 0
  par(lwd = 2)
  for (i in sort(unique(rand))) {
    train <- rand!=i
    test <- rand==i
    n.in <- (1:n)[train]
    n.out <- (1:n)[test]
    if (printit) {
      cat("\nfold", i, "\n")
      cat("Observations in test set:", n.out, "\n")
    }
    df.lm <- lm(form.lm, data=subset(df, train))
    pred <- predict(df.lm, newdata=subset(df, test))
    resid <- yv[n.out] - pred
    if(!multilr){
      z <- xv[n.out]
      xy <- data.frame(rbind(z, pred, yv[n.out], resid),
                       row.names = c(paste("x=", xnam, sep=""),
                       "Predicted", ynam, "Residual"))
      ab <- coef(df.lm)
    }
    else {
       z <- allpred[test]
       inpred <- predict(df.lm)
       ab <- coef(lm(inpred ~ allpred[train]))
       xy <- data.frame(rbind(pred, yv[n.out], resid),
                       row.names = c("Predicted", ynam, "Residual"))
     }
    if (plotit) {
      points(z, yv[n.out], col = coltypes[i], pch = ptypes[i], 
             cex = 1.25)
      if (dots) 
        points(z, yv[n.out], col = coltypes[i], pch = 16)
      abline(ab, lwd = 2, col = coltypes[i], lty = ltypes[i])
    }
    num <- length(n.out)
    if (printit) 
      print(xy, collab = rep("", num))
    ss <- sum(resid^2)
    sumss <- sumss + ss
    sumdf <- sumdf + num
    ms <- ss/num
    if (printit) 
      cat("\nSum of squares =", signif(ss, 2), "   Mean square =", 
          signif(ms, 2), "   n =", num, "\n")
  }
  if (printit) 
    print(c("Overall ms" = sumss/sumdf))
  if (plotit) {
    par(col = 1)
  }
  invisible(c(ss = sumss, df = sumdf, foldinfo=rand))
}

