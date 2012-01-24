bsnCV <-
function (m = 100, n = 40, method = "exhaustive", nvmax = 3, X=NULL,
              nfolds = 2, print.summary = TRUE, really.big = FALSE)
{
    leaps.out <- try(require(leaps), silent = TRUE)
    if (!is.logical(leaps.out) | (leaps.out == FALSE)) {
        print("Error: package leaps is not installed properly")
        return()
    }
     if(is.null(X)){X <- matrix(rnorm(m * n), ncol = n)
        colnames(X) <- paste("V", 1:n, sep = "")
                  } else
       {X <- as.matrix(X)
           m <- dim(X)[1]
        n <- dim(X)[2]
        if(is.null(colnames(X)))colnames(X) <- paste("V", 1:n, sep = "")
    }
    y <- rnorm(m)
    foldid <- sample(1:nfolds, m, replace = TRUE)
    objlist <- vector("list", length = nfolds)
    for (i in 1:nfolds) {
        train <- foldid != i
        test <- !train
        xxi <- X[train, ]
        yi <- y[train]
        u <- regsubsets(xxi, yi, method = method, nvmax = nvmax,
                        nbest = 1, really.big = really.big)
        x <- X[test, summary(u)$which[nvmax, -1]]
        objlist[[i]] <- lm(y[test] ~ x)
    }
    if (print.summary)
        for (i in 1:nfolds) print(summary(objlist[[i]]))
    invisible(objlist)
}
