\name{science}
\alias{science}
\non_function{}
\title{School Science Survey Data}
\usage{science}
\description{
The \code{science} data frame has 1385 rows and 7 columns.

The data are on attitudes to science, from a survey where there were
results from 20 classes in private schools and 46 classes in public
schools.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{State}{a factor with levels
   \code{ACT} Australian Capital Territory, 
   \code{NSW} New South Wales}
    \item{PrivPub}{a factor with levels
        \code{private} school, 
        \code{public} school}
    \item{school}{a factor, coded to identify the school}
    \item{class}{a factor, coded to identify the class}
    \item{sex}{a factor with levels
    \code{f}, \code{m} }
    \item{like}{ a summary score
  based on two of the questions, on a scale from 1 (dislike)
  to 12 (like)}
    \item{Class}{a factor with levels corresponding to each class}
}
}
\source{
 Francine Adams,  Rosemary Martin and Murali Nayadu, Australian
National University
}
\examples{
attach(science)
classmeans <- aggregate(like, by=list(PrivPub, Class), mean)
names(classmeans) <- c("PrivPub","Class","like")
dim(classmeans)

attach(classmeans)
boxplot(split(like, PrivPub), ylab = "Class average of attitude to science score", boxwex = 0.4)
rug(like[PrivPub == "private"], side = 2)
rug(like[PrivPub == "public"], side = 4)
detach(classmeans)

require(nlme)
science.lme <- lme(fixed = like ~ sex + PrivPub,
  data = science, random = ~ 1 | school/Class, na.action=na.omit)
summary(science.lme)$tTable # Print coefficients.

science1.lme <- lme(fixed = like ~ sex + PrivPub, data = science,
random = ~ 1 | Class, na.action=na.exclude)
summary(science1.lme)$tTable # Table of coefficients

intervals(science1.lme, which="var-cov")[[1]]$Class^2
intervals(science1.lme, which="var-cov")[[2]]^2 

science.lme <- lme(fixed = like ~ sex + PrivPub,
  data = science, random =  ~ 1 | Class/school, na.action=na.exclude)
res <- residuals(science.lme)
hat <- fitted(science.lme)
coplot(res ~ hat|science$PrivPub[is.na(science$sex)!=TRUE],
  xlab="Fitted values", ylab="Residuals")
detach(science)
}
\keyword{datasets}
