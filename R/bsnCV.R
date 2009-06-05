bsnCV <-
function (m = 100, n = 40, method = "exhaustive", nvmax = 3, nfolds=2,
            print.summary=TRUE)
{
  leaps.out <- try(require(leaps), silent = TRUE)
  if (!is.logical(leaps.out) | (leaps.out == FALSE)) {
    print("Error: package leaps is not installed properly")
    return()
  }
    y <- rnorm(m)
    xx <- matrix(rnorm(m * n), ncol = n)
    dimnames(xx) <- list(NULL, paste("V", 1:n, sep = ""))
    foldid <- sample(1:nfolds, m, replace=TRUE)
    objlist <- vector("list", length=nfolds)
    for (i in 1:nfolds){
      train <- foldid!=i
      test <- !train
      xxi <- xx[train,]
      yi <- y[train]
      u <- regsubsets(xxi, yi, method = method, nvmax = nvmax, 
                    nbest = 1)
      x <- xx[test, summary(u)$which[nvmax, -1]]
      objlist[[i]] <- lm(y[test] ~ x)
    }
  if(print.summary)
    for(i in 1:nfolds)
      print(summary(objlist[[i]]))
    invisible(objlist)
}

