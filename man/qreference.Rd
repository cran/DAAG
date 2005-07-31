\name{qreference}
\alias{qreference}
\title{Normal QQ Reference Plot}
\description{
This function computes the normal QQ plot for given data and
allows for comparison with normal QQ plots of simulated data.
}
\usage{
qreference(test = NULL, m = 50, nrep = 6, distribution = function(x) qnorm(x, 
    mean = ifelse(is.null(test), 0, mean(test)), sd = ifelse(is.null(test), 
    1, sd(test))), seed = NULL, nrows = NULL, cex.strip = 0.75, 
    xlab = NULL, ylab = NULL) 
}
\arguments{
  \item{test}{a vector containing a sample to be tested; if not supplied,
all qq-plots are of the reference distribution}
  \item{m}{the sample size for the reference samples; default is
test sample size if test sample is supplied}
  \item{nrep}{the total number of samples, including reference
samples and test sample if any}
  \item{distribution}{reference distribution; default is standard normal}
  \item{seed}{the random number generator seed}
  \item{nrows}{number of rows in the plot layout}
  \item{cex.strip}{character expansion factor for labels}
  \item{xlab}{label for x-axis}
  \item{ylab}{label for y-axis}
}
\value{
QQ plots of the sample (if test is non-null) and all reference samples
}
\author{J.H. Maindonald}

\examples{
# qreference(rt(180,1))

# qreference(rt(180,1), distribution=function(x) qt(x, df=1))

# qreference(rexp(180), nrep = 4)

# toycars.lm <- lm(distance ~ angle + factor(car), data = toycars)
# qreference(residuals(toycars.lm), nrep = 9)
}
\keyword{models}
