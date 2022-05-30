## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----LOAD-DAAG----------------------------------------------------------------
library(DAAG, warn.conflicts=FALSE)
library(latticeExtra)

## ----mf, eval=FALSE, echo=FALSE-----------------------------------------------
#  plot(timef~time, data=nihills,
#       xlab="Male record times",
#       ylab="Female record times")
#  mtext("Female vs male record times", side=3, line=0.25)
#  mftime.lm <- lm(timef ~ time, data=nihills)
#  abline(mftime.lm)
#  plot(mftime.lm, which=1)

## ----cap1, echo=F-------------------------------------------------------------
cap1 <- "Record times for hill races are compared -- females versus
  males." 

## ----mf, fig.width=4.0, fig.height=4.25, eval=TRUE, echo=FALSE, out.width="47%", fig.show='hold', fig.cap=cap1----
plot(timef~time, data=nihills,
     xlab="Male record times",
     ylab="Female record times")
mtext("Female vs male record times", side=3, line=0.25)
mftime.lm <- lm(timef ~ time, data=nihills)
abline(mftime.lm)
plot(mftime.lm, which=1)

## ----mf, eval=FALSE, echo=TRUE, tidy=FALSE------------------------------------
#  plot(timef~time, data=nihills,
#       xlab="Male record times",
#       ylab="Female record times")
#  mtext("Female vs male record times", side=3, line=0.25)
#  mftime.lm <- lm(timef ~ time, data=nihills)
#  abline(mftime.lm)
#  plot(mftime.lm, which=1)

## ----cap2, echo=F-------------------------------------------------------------
cap2 <- "Diagnostic plots from the regession of `timef` on `time`."

## ----diag-mftime, fig.width=2.85, fig.height=3.15, eval=TRUE, echo=FALSE, out.width="22%", fig.show='hold', fig.cap=cap2----
plot(mftime.lm, cex.caption=0.8, ask=FALSE)

## ----cap3, echo=F-------------------------------------------------------------
cap3 <- "The plots are four simulations of points.  The coefficients
  used, and the standard deviation, are from the fitted least squares line.  The residuals from the fitted model are in gray."

## ----4sim-xy, fig.width=7.0, fig.height=3.0, eval=TRUE, echo=FALSE, fig.cap=cap3----
mftime.lm <- lm(timef ~ time, data=nihills)
gph <- plotSimScat(mftime.lm, show="residuals")
gph <- update(gph, xlab="Record times for males (h)",
              ylab="Record times for females (h)")
print(gph)

## ----4sim-xy, ref.label="4sim-xy", eval=FALSE, echo=TRUE----------------------
#  mftime.lm <- lm(timef ~ time, data=nihills)
#  gph <- plotSimScat(mftime.lm, show="residuals")
#  gph <- update(gph, xlab="Record times for males (h)",
#                ylab="Record times for females (h)")
#  print(gph)

## ----cap4, echo=F-------------------------------------------------------------
cap4 <- "Residuals versus fitted values, for four sets of simulated data."

## ----4simwhich1, eval=TRUE, echo=FALSE, fig.width=7, fig.height=3.0, fig.cap=cap4----
plotSimDiags(obj=mftime.lm, which=1, layout=c(4,1))

## ----4simwhich1-code, ref.label="4simwhich1", eval=FALSE, echo=TRUE, tidy=FALSE----
#  plotSimDiags(obj=mftime.lm, which=1, layout=c(4,1))

## ----cap5, echo=F-------------------------------------------------------------
cap5 <- "Normal probability plots for four sets of simulated
  data."

## ----4simwhich2, fig.width=7, fig.height=3.0, eval=T, echo=FALSE, fig.cap=cap5----
plotSimDiags(obj=mftime.lm, which=2, layout=c(4,1))

## ----cap6, echo=F-------------------------------------------------------------
cap6 <- "Scale-location plots for four sets of simulated data."

## ----4simwhich3, fig.width=7.0, fig.height=3.0, eval=T, echo=FALSE, fig.cap=cap6----
plotSimDiags(obj=mftime.lm, which=3, layout=c(4,1))

## ----cap7, echo=F-------------------------------------------------------------
cap7 <- "Scale-location plots for four sets of simulated data."

## ----4simwhich5, fig.width=7, fig.height=3.0, eval=T, echo=FALSE, fig.cap=cap7----
plotSimDiags(obj=mftime.lm, which=5, layout=c(4,1))

## ----all4, eval=T, echo=T-----------------------------------------------------
gphs1to4 <- plotSimDiags(obj=mftime.lm, layout=c(4,2))

## ----plot1, eval=F------------------------------------------------------------
#  update(gphs1to4[[1]], layout=c(4,2))

