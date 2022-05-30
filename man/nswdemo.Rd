\name{nswdemo}
\alias{nswdemo}
\docType{data}
\title{Labour Training Evaluation Data}
\usage{data(nswdemo)}
\description{
The \code{nswdemo} data frame contains 722 rows and 10 columns.
These data are pertinent to an investigation of the way that   
earnings changed, between 1974-1975 and 1978, for an experimental
treatment who were given job training as compared with a control
group who did not receive such training.

The \code{psid1} data set is an alternative non-experimental "control"
group. \code{psid2} and \code{psid3} are subsets of \code{psid1},
designed to be better matched to the experimental data than
\code{psid1}. Note also the \code{cps1}, \code{cps2} and \code{cps3}
datasets (\pkg{DAAGxtras}) that have been proposed as
non-experimental controls.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{trt}{a numeric vector 
identifying the study in which the subjects were enrolled
(0 = Control, 1 = treated).}
    \item{age}{ age (in years).}
    \item{educ}{ years of education.}
    \item{black}{ (0 = not black, 1 = black).}
    \item{hisp}{ (0 = not hispanic, 1 = hispanic).}
    \item{marr}{ (0 = not married, 1 = married).}
    \item{nodeg}{ (0 = completed high school, 1 = dropout).}
    \item{re74}{ real earnings in 1974.}
    \item{re75}{ real earnings in 1975.}
    \item{re78}{ real earnings in 1978.} 
  }
}
\source{
  \url{https://users.nber.org/~rdehejia/nswdata.html}
}
\references{
 Dehejia, R.H. and Wahba, S. 1999. Causal effects in
non-experimental studies: re-evaluating the evaluation of training
programs. Journal of the American Statistical Association 94:
1053-1062.

 Lalonde, R. 1986. Evaluating the economic evaluations of
 training programs. American Economic Review 76: 604-620.
 
Smith, J. A. and Todd, P.E. 2005,"Does Matching overcome. LaLonde?s
 critique of nonexperimental estimators", \emph{Journal of
 Econometrics} 125: 305-353.

 Dehejia, R.H. 2005. Practical propensity score matching: a reply to
 Smith and Todd. \emph{Journal of Econometrics} 125: 355-364.
}
\seealso{\code{\link{psid1}}, \code{\link{psid2}}, \code{\link{psid3}}
}
\keyword{datasets}
