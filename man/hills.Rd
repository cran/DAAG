\name{hills}
\alias{hills}
\alias{hills2000}
\docType{data}
\title{Scottish Hill Races Data}
\usage{
  data(hills)
  data(hills2000)
  }
\description{
  The record times in 1984 (\code{hills}) for 35 Scottish hill races,
  or in 2000 (\code{hills2000}) for 56 hill races.  The \code{hills2000}
  dataset is the subset of \code{\link{races2000}} for which \code{type}
  is \code{hill}.
}
\format{
  \describe{
    \item{dist}{distance, in miles (on the map)}
    \item{climb}{total height gained during the route, in feet}
    \item{time}{record time in hours}
    \item{timef}{record time in hours for females, in the
      \code{hills2000} dataset.
    }
  }
}
\source{
 A.C. Atkinson (1986) Comment: Aspects of diagnostic regression
     analysis. Statistical Science  1, 397-402.

     Also, in MASS library, with time in minutes.

The Scottish Running Resource, http://www.hillrunning.co.uk
}
\references{
     A.C. Atkinson (1988) Transformations unmasked. Technometrics 30,
     311-318. [ "corrects" the time for Knock Hill, in the \code{hills}
     dataset, from 78.65 to 18.65. It   
     is unclear if this based on the original records.]
}
\examples{
print("Transformation - Example 6.4.3")
pairs(hills, labels=c("dist\n\n(miles)", "climb\n\n(feet)", 
"time\n\n(hours)"))
pause()

pairs(log(hills), labels=c("dist\n\n(log(miles))", "climb\n\n(log(feet))",
  "time\n\n(log(hours))"))
pause()

hills0.loglm <- lm(log(time) ~ log(dist) + log(climb), data = hills)  
oldpar <- par(mfrow=c(2,2))
plot(hills0.loglm)
pause()


hills.loglm <- lm(log(time) ~ log(dist) + log(climb), data = hills[-18,])
summary(hills.loglm) 
plot(hills.loglm)
pause()

hills2.loglm <- lm(log(time) ~ log(dist)+log(climb)+log(dist):log(climb), 
data=hills[-18,])
anova(hills.loglm, hills2.loglm)
pause()

step(hills2.loglm)
pause()

summary(hills.loglm, corr=TRUE)$coef
pause()

summary(hills2.loglm, corr=TRUE)$coef
par(oldpar)
pause()

print("Nonlinear - Example 6.9.4")
hills.nls0 <- nls(time ~ (dist^alpha)*(climb^beta), start =
   c(alpha = .909, beta = .260), data = hills[-18,])
summary(hills.nls0)
plot(residuals(hills.nls0) ~ predict(hills.nls0)) # residual plot
pause()

hills$climb.mi <- hills$climb/5280
hills.nls <- nls(time ~ alpha + beta*dist + gamma*(climb.mi^delta),
  start=c(alpha = 1, beta = 1, gamma = 1, delta = 1), data=hills[-18,])
summary(hills.nls)
plot(residuals(hills.nls) ~ predict(hills.nls)) # residual plot


}
\keyword{datasets}
