\name{jobs}
\alias{jobs}
\docType{data}
\title{Canadian Labour Force Summary Data (1995-96)}
\usage{jobs}
\description{
The number of workers in the Canadian
labour force broken down by region (BC, Alberta, Prairies,
Ontario, Quebec, Atlantic) for the 24-month period from January,
1995 to December, 1996 (a time when Canada was emerging from a
deep economic recession).
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{BC}{monthly labour force counts in British Columbia}
    \item{Alberta}{monthly labour force counts in Alberta}
    \item{Prairies}{monthly labour force counts in Saskatchewan and 
Manitoba}
    \item{Ontario}{monthly labour force counts in Ontario}
    \item{Quebec}{monthly labour force counts in Quebec}
    \item{Atlantic}{monthly labour force counts in Newfoundland, Nova 
Scotia, Prince Edward Island and New Brunswick}
    \item{Date}{year (in decimal form)}
  }
}
\details{
These data have been seasonally adjusted.
}
\source{
Statistics Canada
}
\examples{
print("Multiple Variables and Times - Example 2.1.4")
sapply(jobs, range)
pause()

matplot(jobs[,7], jobs[,-7], type="l", xlim=c(95,97.1))
 # Notice that we have been able to use a data frame as the second argument to matplot().
 # For more information on matplot(), type help(matplot)
text(rep(jobs[24,7], 6), jobs[24,1:6], names(jobs)[1:6], adj=0)
pause()

sapply(log(jobs[,-7]), range)
apply(sapply(log(jobs[,-7]), range), 2, diff)
pause()

oldpar <- par(mfrow=c(2,3))
range.log <- sapply(log(jobs[,-7], 2), range)
maxdiff <- max(apply(range.log, 2, diff))
range.log[2,] <- range.log[1,] + maxdiff
titles <- c("BC Jobs","Alberta Jobs","Prairie Jobs",
   "Ontario Jobs", "Quebec Jobs", "Atlantic Jobs")
for (i in 1:6){
plot(jobs$Date, log(jobs[,i], 2), type = "l", ylim = range.log[,i],
    xlab = "Time", ylab = "Number of jobs", main = titles[i])
}
par(oldpar)
}
\keyword{datasets}
