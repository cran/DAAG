\name{science}
\alias{science}
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
classmeans <- with(science, aggregate(like, by=list(PrivPub, Class), mean))
names(classmeans) <- c("PrivPub","Class","like")
dim(classmeans)

attach(classmeans)
boxplot(split(like, PrivPub), ylab = "Class average of attitude to science score", boxwex = 0.4)
rug(like[PrivPub == "private"], side = 2)
rug(like[PrivPub == "public"], side = 4)
detach(classmeans)
if(require(lme4, quietly=TRUE)) {
science.lmer <- lmer(like ~ sex + PrivPub + (1 | school) +
                     (1 | school:class), data = science,
                     na.action=na.exclude)
summary(science.lmer)
science1.lmer <- lmer(like ~ sex + PrivPub + (1 | school:class),
                      data = science, na.action=na.exclude)
summary(science1.lmer)
ranf <- ranef(obj = science1.lmer, drop=TRUE)[["school:class"]]
flist <- science1.lmer@flist[["school:class"]]
privpub <- science[match(names(ranf), flist), "PrivPub"]
num <- unclass(table(flist)); numlabs <- pretty(num)
## Plot effect estimates vs numbers
plot(sqrt(num), ranf, xaxt="n", pch=c(1,3)[as.numeric(privpub)],
     xlab="# in class (square root scale)",
     ylab="Estimate of class effect")
lines(lowess(sqrt(num[privpub=="private"]),
             ranf[privpub=="private"], f=1.1), lty=2)
lines(lowess(sqrt(num[privpub=="public"]),
             ranf[privpub=="public"], f=1.1), lty=3)
axis(1, at=sqrt(numlabs), labels=paste(numlabs))
}
}
\keyword{datasets}
