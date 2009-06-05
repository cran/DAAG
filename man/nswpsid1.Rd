\name{nswpsid1}
\docType{data}
\alias{nswpsid1}
\title{Labour Training Evaluation Data}
\usage{psid1}
\description{
  This data frame contains 2787 rows and 10 columns. These data are
  pertinent to an investigation of the way that earnings changed,
  between 1974-1975 and 1978, in the absence of training. Data for the
  experimental treatment group in \code{nswdemo} are combined with the
  \code{psid1} control data from the Panel Study of Income Dynamics
  (PSID) study.
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
\details{ 
The \code{cps1} and \code{psid1} data sets are two non-experimental
"control" groups, alternative to that in \code{nswdemo}, used in
investigating whether use of such a non-experimental control group can
be satisfactory. \code{cps2} and \code{cps3} are subsets of \code{cps1},
designed to be better matched to the experimental data than \code{cps1}.
Similary \code{psid2} and \code{psid3} are subsets of \code{psid1},
designed to be better matched to the experimental data than
\code{psid1}. \code{nswpsid1} combines data for the experimental
treatment group in \code{nswdemo} with the \code{psid1} control data
from the Panel Study of Income Dynamics (PSID) study.
}
\source{
  \url{http://www.nber.org/~rdehejia/nswdata.html}
}
\references{
 Dehejia, R.H. and Wahba, S. 1999. Causal effects in
non-experimental studies: re-evaluating the evaluation of training
programs. \emph{Journal of the American Statistical Association} 94:
1053-1062.

 Lalonde, R. 1986. Evaluating the economic evaluations of
 training programs. \emph{American Economic Review} 76: 604-620.

 Smith, J. A. and Todd, P.E. "Does Matching overcome. LaLonde?s
 critique of nonexperimental estimators", \emph{Journal of
 Econometrics} 125: 305-353.

 Dehejia, R.H. 2005. Practical propensity score matching: a reply to
 Smith and Todd. \emph{Journal of Econometrics} 125: 355-364.
}
\keyword{datasets}