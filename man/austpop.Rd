\name{austpop}
\alias{austpop}
\docType{data}
\title{Population figures for Australian States and Territories}
\usage{austpop}
\description{
Population figures for Australian states and territories for 1917, 1927,
..., 1997.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{year}{a numeric vector}
    \item{NSW}{New South Wales population counts }
    \item{Vic}{Victoria population counts}
    \item{Qld}{Queensland population counts}
    \item{SA}{South Australia population counts}
    \item{WA}{Western Australia population counts}
    \item{Tas}{Tasmania population counts}
    \item{NT}{Northern Territory population 
counts}
    \item{ACT}{Australian Capital Territory 
population counts}
    \item{Aust}{Population counts for
the whole country}
  }
}
\source{
Australian Bureau of Statistics
}
\examples{
print("Looping - Example 1.7")

growth.rates <- numeric(8)
for (j in seq(2,9)) {
    growth.rates[j-1] <- (austpop[9, j]-austpop[1, j])/austpop[1, j] }
growth.rates <- data.frame(growth.rates)
row.names(growth.rates) <- names(austpop[c(-1,-10)])
  # Note the use of row.names() to name the rows of the data frame
growth.rates

pause()
print("Avoiding Loops - Example 1.7b")

sapply(austpop[,-c(1,10)], function(x){(x[9]-x[1])/x[1]})

pause()
print("Plot - Example 1.8a")
attach(austpop)
plot(year, ACT, type="l") # Join the points ("l" = "line")
detach(austpop)

pause()
print("Exerice 1.12.9")
attach(austpop)
oldpar <- par(mfrow=c(2,4))  
for (i in 2:9){
plot(austpop[,1], log(austpop[, i]), xlab="Year",
    ylab=names(austpop)[i], pch=16, ylim=c(0,10))}
par(oldpar) 
detach(austpop)

}
\keyword{datasets}
