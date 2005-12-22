"compareTreecalcs" <-
function(x=yesno~., data=spam7, cp=0.00025,
           fun=c("rpart","randomForest")){
    m <- dim(data)[1]
    train <- sample((1:m), m%/%2)
    dftrain <- data[train,]
    dftest <- data[-train,]
    if("rpart"%in%fun){
      df.rpart <- rpart(x, data=dftrain, cp=cp)
      cptable <- df.rpart$cptable
      err.root <- df.rpart$frame$dev[1]/df.rpart$frame$n[1]
      xerror <- cptable[, "xerror"]
      xstd <- cptable[, "xstd"]
      CP <- cptable[, "CP"]
      nREmin <- as.numeric(which.min(xerror))
      if(nREmin!=length(xerror)){
        re.min <- xerror[nREmin]
        cv.min <- as.vector(re.min*err.root)
        selim <- min(xerror+xstd)
        nSErule <- min(seq(along=xerror)[xerror<=selim])
        cv.selim <- as.vector(err.root*xerror[nSErule])
        cp.selim <- mean(CP[(nSErule-1):nSErule])
        cp.remin <- mean(CP[(nREmin-1):nREmin])
        df.rpart <- prune(df.rpart, cp=cp.remin)
        hat <- predict(df.rpart, newdata=dftest, type="class")
        tab <- table(hat, dftest$yesno)
        testerr.estmin <- sum(tab[row(tab)!=col(tab)])/sum(tab)
        df.rpart <- prune(df.rpart, cp=cp.selim)
        hat <- predict(df.rpart, newdata=dftest, type="class")
        tab <- table(hat, dftest$yesno)
        testerr.SE <- sum(tab[row(tab)!=col(tab)])/sum(tab)
      } else
      {
        cv.selim <- NA
        cv.min <- NA
        testerr.SE <- NA
        testerr.estmin <- NA
        nSErule <- NA
        nREmin <- NA
      }
    } else
    {cv.selim <- NULL
     cv.min <- NULL
     testerr.SE <- NULL
     testerr.estmin <- NULL
     nSErule <- NULL
     nREmin <- NULL
   }
    ## randomForest
    if("randomForest"%in%fun){
      yvar <- all.vars(x)[1]
      y <- dftrain[, yvar]
      ynum <- match(yvar, names(dftrain))
      df.rf <- randomForest(x = dftrain[, -ynum], y=y)
      hat.dfcv <- predict(df.rf, type="response")
      tab <- table(hat.dfcv, dftrain$yesno)
      rfcvA <- sum(tab[row(tab)!=col(tab)])/sum(tab)
      hat.dftest <- predict(df.rf, newdata=dftest[,-ynum], type="response")
      tab <- table(hat.dftest, dftest$yesno)
      rftest <- sum(tab[row(tab)!=col(tab)])/sum(tab)
    }
    else
      {
        rfcvA <- NULL
        rftest <- NULL
      }
    c(rpSEcvI=cv.selim, rpcvI= cv.min, rpSEtest=testerr.SE,
      rptest=testerr.estmin, nSErule=nSErule, nREmin=nREmin, rfcvI=rfcvA,
      rftest=rftest)
  }

