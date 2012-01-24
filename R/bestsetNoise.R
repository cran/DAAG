bestsetNoise <-
function (m = 100, n = 40, method = "exhaustive", nvmax = 3,
              X=NULL, print.summary = TRUE, really.big=FALSE)
{
    leaps.out <- try(require(leaps), silent = TRUE)
    if ((is.logical(leaps.out) == TRUE) & (leaps.out == TRUE)) {
        if(is.null(X)){X <- matrix(rnorm(m * n), ncol = n)
                       colnames(X) <- paste("V", 1:n, sep = "")
                   } else
    {X <- as.matrix(X)
     m <- dim(X)[1]
     n <- dim(X)[2]
     if(is.null(colnames(X)))colnames(X) <- paste("V", 1:n, sep = "")
 }
        y <- rnorm(m)
        u <- regsubsets(X, y, method = method, nvmax = nvmax,
                        nbest = 1, really.big=really.big)
        x <- X[, summary(u)$which[nvmax, -1]]
        u1 <- lm(y ~ x)
        if (print.summary)
            print(summary(u1, corr = FALSE))
        invisible(u1)
    }
    else {
        print("Error: package leaps is not installed properly")
    }
}
