\name{qreference}
\alias{qreference}
\title{Normal QQ Reference Plot}
\description{
This function computes the normal QQ plot for given data and
allows for comparison with normal QQ plots of simulated data.
}
\usage{
qreference(test=NULL, mu = 10, sigma = 1, m = 50, nrep = 5,
             seed=NULL, nrows=NULL, cex.points=0.65, cex.strip=0.75)
}
\arguments{
  \item{test}{a vector containing a sample to be tested}
  \item{mu}{the mean of the simulated reference samples}
  \item{sigma}{the standard deviation of the reference samples}
  \item{m}{the sample size for the reference samples; default is
test sample size if test sample is supplied}
  \item{nrep}{the total number of samples, including reference
samples and test sample if any}
  \item{seed}{the random number generator seed}
  \item{nrows}{number of rows in the plot layout}
  \item{cex.points}{character expansion factor for plotted points}
  \item{cex.strip}{character expansion factor for labels}
}
\value{
QQ plots of the sample (if non-null) and all reference samples
}
\author{J.H. Maindonald}

\examples{
qreference(rt(180,1))

qreference(rexp(180), nrep = 4)

data(toycars)
toycars.lm <- lm(distance ~ angle + factor(car), data = toycars)
qreference(residuals(toycars.lm), nrep = 9)
}
\keyword{models}
