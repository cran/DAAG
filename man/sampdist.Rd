\name{sampdist}
\Rdversion{1.1}
\alias{sampdist}
\title{
Plot sampling distribution of mean or other sample statistic.
}
\description{
  The function \code{sampvals} generates the data.  A density plot
  of a normal probability plot is provided, for one or mare sample
  sizes.  For a density plot, the density estimate for the population
  is superimposed in gray.  For the normal probability plot, the
  population plot is a dashed gray line. Default arguments give the
  sampling distribution of the mean, for a distribution that is
  mildly positively skewed.
}
\usage{
sampdist(sampsize = c(3, 9, 30), seed = NULL, nsamp = 1000, FUN = mean,
         sampvals = function(n) exp(rnorm(n, mean = 0.5, sd = 0.3)),
         tck = NULL, plot.type = c("density", "qq"), layout = c(3, 1))
}
\arguments{
  \item{sampvals}{
    Function that generates the data.  For sampling from existing data
    values, this might be function that generates bootstrap samples.
}
  \item{sampsize}{
    One or more sample sizes.  A plot will be provided for each different
    sample size.
}
  \item{seed}{
    Specify a seed if it is required to make the exact set(s) of sample
    values reproducible.
}
  \item{nsamp}{
Number of samples.
}
  \item{FUN}{
Function that calculates the sample statistic.
}
  \item{plot.type}{
    Specify \code{density}, or \code{qq}.  Or if no plot is required,
    specify \code{""}.
}
  \item{tck}{
    Tick size on lattice plots, by default 1, but 0.5 may be suitable
    for plots that are, for example, 50\% of the default dimensions
    in each direction.
}
\item{layout}{Layout on page, e.g. \code{c(3,1)} for a 3 columns
  by one row layout.
}
}
\value{
Data frame
}
\author{
John Maindonald.
}
\examples{
sampdist(plot.type="density")
sampdist(plot.type="qq")

## The function is currently defined as
  function (sampsize = c(3, 9, 30), seed = NULL, nsamp = 1000, FUN = mean,
            sampvals = function(n) exp(rnorm(n, mean = 0.5, sd = 0.3)),
            tck = NULL, plot.type = c("density", "qq"), layout = c(3,
                                                          1))
{
  if (!is.null(seed))
    set.seed(seed)
  ncases <- length(sampsize)
  y <- sampvals(nsamp)
  xlim = quantile(y, c(0.01, 0.99))
  xlim <- xlim + c(-1, 1) * diff(xlim) * 0.1
  samplingDist <- function(sampsize=3, nsamp=1000, FUN=mean)
    apply(matrix(sampvals(sampsize*nsamp), ncol=sampsize), 1, FUN)
  df <- data.frame(sapply(sampsize, function(x)samplingDist(x, nsamp=nsamp)))
  names(df) <- paste("y", sampsize, sep="")
  form <- formula(paste("~", paste(names(df), collapse="+")))
  lab <- lapply(sampsize, function(x) substitute(A, list(A = paste(x))))
  if (plot.type[1] == "density")
    gph <- densityplot(form, data=df, layout = layout, outer=TRUE,
                       plot.points = FALSE, panel = function(x, ...) {
                         panel.densityplot(x, ..., col = "black")
                         panel.densityplot(y, col = "gray40", lty = 2,
                                           ...)
                       }, xlim = xlim, xlab = "", scales = list(tck = tck),
                       between = list(x = 0.5), strip = strip.custom(strip.names = TRUE,
                       factor.levels = as.expression(lab), var.name = "Sample size",
                                                  sep = expression(" = ")))
  else if (plot.type[1] == "qq")
    gph <- qqmath(form, data = df, layout = layout, plot.points = FALSE,
                  outer=TRUE,
                  panel = function(x, ...) {
                    panel.qqmath(x, ..., col = "black", alpha=0.5)
                    panel.qqmath(y, col = "gray40", lty = 2, type = "l",
                                 ...)
                  }, xlab = "", xlim = c(-3, 3), ylab = "", scales = list(tck = tck),
                  between = list(x = 0.5), strip = strip.custom(strip.names = TRUE,
                  factor.levels = as.expression(lab), var.name = "Sample size",
                                             sep = expression(" = ")))
  if (plot.type[1] \%in\% c("density", "qq"))
    print(gph)
  invisible(df)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}

