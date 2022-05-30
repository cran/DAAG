\name{nsw74demo}
\alias{nsw74demo}
\alias{nsw74psid1}
\alias{nsw74psid3}
\alias{nsw74psidA}
\docType{data}
\title{Labour Training Evaluation Data}
\usage{
  data(nsw74demo)
  data(nsw74psid1)
  data(nsw74psid3)
  data(nsw74psidA)
}
\description{
  This \code{nsw74demo} data frame, with 445 rows and 10 columns,
  is the subset of the \code{\link{nswdemo}} dataset for which 1974
  earnings are available. 
  Data are for the male experimental control and treatment
  groups, in an investigation of the effect of training
  on changes, between 1974-1975 and 1978, in the earnings
  of individuals who had experienced employment difficulties.

  Likewise, \code{nsw74psid1} (2675 rows) is the subset of the
  \code{nswpsid1} data, and \code{nsw74psid3} (313 rows) is the subset of
  the  \code{nswpsid3} data, for which 1974 income is available.
  NB, also, the \code{nsw74psidA} data set.
}
\details{
  The \code{nsw74psidA} data set (252 rows) was obtained from
  \code{nsw74psid1} using:

\code{here <- age <= 40 & re74<=5000 & re75 <= 5000 & re78 < 30000}

\code{nsw74psidA <- nsw74psid1[here, ]}
}
\format{
  Columns are:
  \describe{
    \item{trt}{a numeric vector 
identifying the study in which the subjects were enrolled
(0 = PSID, 1 = NSW).}
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
  http://www.columbia.edu/~rd247/nswdata.html
}
\references{
 Dehejia, R.H. and Wahba, S. 1999. Causal effects in
non-experimental studies: re-evaluating the evaluation of training
programs. Journal of the American Statistical Association 94:
1053-1062.

 Lalonde, R. 1986. Evaluating the economic evaluations of
training programs. American Economic Review 76: 604-620.
}

\keyword{datasets}
