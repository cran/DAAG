\name{seedrates}
\alias{seedrates}
\non_function{}
\title{Barley Seeding Rate Data}
\usage{data(seedrates)}
\description{
The \code{seedrates} data frame has 5 rows and 2 columns on 
the effect of seeding rate of barley on yield.

}
\format{
  This data frame contains the following columns:
  \describe{
    \item{rate}{the seeding rate}
    \item{grain}{the number of grain per head of barley}
  }
}
\source{
 McLeod, C.C. 1982.
Effect of rates of seeding on barley grown for grain. New Zealand 
Journal of Agriculture 10: 133-136.}
\references{
 Maindonald J H 1992. Statistical design, analysis and presentation
issues. New Zealand Journal of Agricultural Research 35: 121-141.}
\examples{
data(seedrates)
plot(grain~rate,data=seedrates,xlim=c(50,180),ylim=c(15.5,22),axes=FALSE)
new.df<-data.frame(rate=(2:8)*25)
seedrates.lm1<-lm(grain~rate,data=seedrates)
seedrates.lm2<-lm(grain~rate+I(rate^2),data=seedrates)
hat1<-predict(seedrates.lm1,newdata=new.df,interval="confidence")
hat2<-predict(seedrates.lm2,newdata=new.df,interval="confidence")
axis(1,at=new.df$rate); axis(2); box()
z1<-spline(new.df$rate, hat1[,"fit"]); z2<-spline(new.df$rate,   
hat2[,"fit"])
rate<-new.df$rate; lines(z1$x,z1$y)
lines(spline(rate,hat1[,"lwr"]),lty=1,col=3)
lines(spline(rate,hat1[,"upr"]),lty=1,col=3)
lines(z2$x,z2$y,lty=4)
lines(spline(rate,hat2[,"lwr"]),lty=4,col=3)
lines(spline(rate,hat2[,"upr"]),lty=4,col=3)}
\keyword{datasets}
