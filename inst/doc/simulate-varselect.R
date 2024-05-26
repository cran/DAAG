## ----setup--------------------------------------------------------------------
knitr::opts_chunk$set(echo=FALSE)

## ----loadDAAG-----------------------------------------------------------------
suppressMessages(library(DAAG, quietly=TRUE, warn.conflicts=FALSE))

## ----bestset, eval=FALSE, echo=TRUE-------------------------------------------
#  bestsetNoise(m=100, n=40, nvmax=3)
#  bestsetNoise(m=100, n=40, method="backward", nvmax=3)

## ----cap1, echo=F-------------------------------------------------------------
cap1 <- "$p$-values from the R function  `lm()`, versus number of
  variables available for selection."

## ----exhaust, eval=TRUE, echo=FALSE-------------------------------------------
## Code
suppressPackageStartupMessages(library(qgam, quietly=TRUE))
set.seed(37)   # Use to reproduce graph that is shown
bsnVaryNvar(m=100, nvar=3:50, nvmax=3)

## ----exhaust, eval=FALSE, echo=TRUE-------------------------------------------
#  ## Code
#  suppressPackageStartupMessages(library(qgam, quietly=TRUE))
#  set.seed(37)   # Use to reproduce graph that is shown
#  bsnVaryNvar(m=100, nvar=3:50, nvmax=3)

