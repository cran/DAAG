\name{qreference}
\alias{qreference}
\title{Simulate QQ reference plots}
\description{
This function computes the QQ plot for given data and specified distribution,
then repeating the comparison for data simulated from the specified 
distribution.  The plots for simulated data give an indication of the
range of variation that is to expected, and thus calibrate the eye.
}
\usage{
qreference(test = NULL, m = 30, nrep = 6, pch=c(16,2), distribution = function(x) qnorm(x, 
    mean = ifelse(is.null(test), 0, mean(test)), sd = ifelse(is.null(test), 
    1, sd(test))), seed = NULL, nrows = NULL, cex.strip = 0.75, 
    xlab = NULL, ylab = NULL) 
}
\arguments{
  \item{test}{a vector containing a sample to be tested; if not supplied,
all qq-plots are for data simulated from the reference distribution}
  \item{m}{the sample size for the reference samples; default is
test sample size if test sample is supplied}
  \item{nrep}{the total number of samples, including reference
samples and test sample if any}
  \item{pch}{plot character(s)}
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
# qreference(rt(30,4))

# qreference(rt(30,4), distribution=function(x) qt(x, df=4))

# qreference(rexp(30), nrep = 4)

# toycars.lm <- lm(distance ~ angle + factor(car), data = toycars)
# qreference(residuals(toycars.lm), nrep = 9)
}
\keyword{models}
