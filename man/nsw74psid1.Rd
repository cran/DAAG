\name{nsw74psid1}
\alias{nsw74psid1}
\non_function{}
\title{Labour Training Evaluation Data}
\usage{nsw74psid3}
\description{
This data frame contains 2675 rows and 10 columns.
These data are pertinent to an investigation of the way that   
earnings changed, between 1974-1975 and 1978, in the absence
of training.  Data for the experimental treatment 
group (NSW) were combined with control data results from the 
Panel Study of Income Dynamics (PSID) study.  
}
\format{
  This data frame contains the following columns:
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
\examples{
print("Interpretation of Regression Coefficients - Example 6.6")

 nsw74psid1.lm <- lm(re78~ trt+ (age + educ + re74 + re75) +
   (black + hisp + marr + nodeg), data = nsw74psid1)
 summary(nsw74psid1.lm)$coef
options(digits=4)
sapply(nsw74psid1[, c(2,3,8,9,10)], quantile, prob=c(.25,.5,.75,.95,1))
attach(nsw74psid1)
sapply(nsw74psid1[trt==1, c(2,3,8,9,10)], quantile, 
prob=c(.25,.5,.75,.95,1))
pause()

here <- age <= 40 & re74<=5000 & re75 <= 5000 & re78 < 30000 
nsw74psidA <- nsw74psid1[here, ]
detach(nsw74psid1)
table(nsw74psidA$trt)
pause()

A1.lm <- lm(re78 ~ trt + (age + educ + re74 + re75) + (black +
      hisp + marr + nodeg), data = nsw74psidA)
summary(A1.lm)$coef
pause()

A2.lm <- lm(re78 ~ trt + (age + educ + re74 + re75) * (black +   
      hisp + marr + nodeg), data = nsw74psidA)
anova(A1.lm, A2.lm)

}
\keyword{datasets}
