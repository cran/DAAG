\name{bomsoi}
\alias{bomsoi}
\non_function{}
\title{Southern Oscillation Index Data}
\usage{bomsoi}
\description{
The Southern Oscillation Index (SOI) is the difference in barometric  
pressure at sea level between Tahiti and Darwin.  Annual SOI and
Australian rainfall data, for the years 1900-2001, are given.
Australia's annual mean rainfall is an area-weighted average of the total 
annual precipitation at approximately 370 rainfall stations 
around the country.

}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Year}{a numeric vector}
\item{Jan}{average January SOI values for each year}
\item{Feb}{average February SOI values for each year}
\item{Mar}{average March SOI values for each year}
\item{Apr}{average April SOI values for each year}
\item{May}{average May SOI values for each year}
\item{Jun}{average June SOI values for each year}
\item{Jul}{average July SOI values for each year}
\item{Aug}{average August SOI values for each year}
\item{Sep}{average September SOI values for each year}
\item{Oct}{average October SOI values for each year}
\item{Nov}{average November SOI values for each year}
\item{Dec}{average December SOI values for each year}
\item{SOI}{a numeric vector consisting of average annual SOI
values}
\item{avrain}{a numeric vector consisting of a weighted average annual
rainfall at a large number of Australian sites}
  } 
} 
  \source{ 
Australian Bureau of Meteorology web pages:

http://www.bom.gov.au/climate/change/rain02.txt and
http://www.bom.gov.au/climate/current/soihtm1.shtml
} 
  \references{
 Nicholls, N., Lavery, B., Frederiksen, C.\ and Drosdowsky, W. 1996.
Recent apparent changes in relationships between the El Nino --
southern oscillation and Australian rainfall and temperature.
Geophysical Research Letters 23: 3357-3360.
} 
  \examples{ 
require(ts)
plot(ts(bomsoi[, 15:14], start=1900),
     panel=function(y,...)panel.smooth(1900:2001, y,...))

# Check for skewness by comparing the normal probability plots for 
# different a, e.g.
par(mfrow = c(2,3))
for (a in c(50, 100, 150, 200, 250, 300))
qqnorm(log(bomsoi[, "avrain"] - a))
  # a = 250 leads to a nearly linear plot
par(mfrow = c(1,1))

plot(bomsoi$SOI, log(bomsoi$avrain - 250), xlab = "SOI",
     ylab = "log(avrain = 250)")
lines(lowess(bomsoi$SOI)$y, lowess(log(bomsoi$avrain - 250))$y, lwd=2)
  # NB: separate lowess fits against time
lines(lowess(bomsoi$SOI, log(bomsoi$avrain - 250)))

detsoi <- data.frame(
  detSOI = bomsoi[, "SOI"] - lowess(bomsoi[, "SOI"])$y,
  detrain = log(bomsoi$avrain - 250) - lowess(log(bomsoi$avrain - 250))$y)
row.names(detsoi) <- paste(1900:2001)

par(mfrow = c(1,2))  
plot(log(avrain-250) ~ SOI, data = bomsoi, ylab = 
 "log(Average rainfall - 250)")
lines(lowess(bomsoi$SOI, log(bomsoi$avrain-250)))
plot(detrain ~ detSOI, data = detsoi,
  xlab="Detrended SOI", ylab = "Detrended log(Rainfall-250)")
lines(lowess(detsoi$detrain ~ detsoi$detSOI)) 
par(mfrow = c(1,1))

require(nlme)
soi.gls <- gls(detrain ~ detSOI, data = detsoi, correlation = 
corARMA(q=12))
summary(soi.gls)

soi1ML.gls <- update(soi.gls, method = "ML")
soi0ML.gls <- update(soi.gls, detrain ~ 1, method = "ML")
soi2ML.gls <- update(soi.gls, detrain ~ detSOI + detSOI^2, method = "ML")
anova(soi2ML.gls, soi1ML.gls)

# compare with MA(11) and MA(13)
soi11.gls <- update(soi.gls, correlation=corARMA(q=11))
soi13.gls <- update(soi.gls, correlation=corARMA(q=13))
anova(soi11.gls, soi.gls, soi13.gls)

# compare with the white noise model
soi0.gls <- gls(detrain ~ detSOI, data=detsoi)
anova(soi0.gls, soi.gls)

# a Portmanteau test of whiteness for the white noise model residuals

Box.test(resid(soi0.gls), lag=20, type="Ljung-Box")
# check residual properties
acf(resid(soi.gls))                     # (Correlated) residuals
acf(resid(soi.gls, type="normalized"))  # Innovation estimates, uncorrelated
qqnorm(resid(soi.gls, type="normalized"))  ## Examine  normality
# Now extract the moving average parameters, and plot the
# the theoretical autocorrelation function that they imply.
beta <- summary(soi.gls$modelStruct)$corStruct
plot(ARMAacf(ma=beta,lag.max=20), type="h")   
# Next, plot several simulated autocorrelation functions
# We can plot autocorrelation functions as though they were time series!

plot.ts(ts(cbind(
  "Series 1" = acf(arima.sim(list(ma=beta), n=83), plot=FALSE, 
lag.max = 20)$acf,
  "Series 2" = acf(arima.sim(list(ma=beta), n=83), plot=FALSE, 
lag.max = 20)$acf,
  "Series 3" = acf(arima.sim(list(ma=beta), n=83), plot=FALSE, 
lag.max = 20)$acf), start=0), type="h", main = "", xlab = "Lag")
# Show confidence bounds for the MA parameters
intervals(soi.gls)

} 
\keyword{datasets}
