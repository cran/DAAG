\name{rice}
\alias{rice}
\non_function{}
\title{Genetically Modified and Wild Type Rice Data}
\usage{data(rice)}
\description{
The \code{rice} data frame has 72 rows and 5 columns.
The data are from an experiment that compared wild type (wt)
and genetically modified rice plants (ANU843), each
with three different chemical treatments (F10, NH4Cl, and NH4NO3).   
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{PlantNo}{a numeric vector}
    \item{Block}{a numeric vector}
    \item{RootDryMass}{a numeric vector}
    \item{ShootDryMass}{a numeric vector}
    \item{trt}{a factor with levels
\code{F10},
\code{NH4Cl},
\code{NH4NO3},
\code{F10 +ANU843},
\code{NH4Cl +ANU843},
\code{NH4NO3 +ANU843}}
  }
}
\source{
 Perrine, F.M., Prayitno, J., Weinman, J.J., Dazzo, F.B. and
Rolfe, B. 2001.  Rhizobium plasmids are involved in the inhibition or
stimulation of rice growth and development.  Australian Journal of
Plant Physiology 28: 923-927.
}
\examples{
data(rice)
print("One and Two-Way Comparisons - Example 4.5")
attach(rice)
oldpar <- par(las = 2)
stripchart(ShootDryMass ~ trt, pch=1, cex=1, xlab="Level of factor 1")
detach(rice)
pause()

rice.aov <- aov(ShootDryMass ~ trt, data=rice); anova(rice.aov)
anova(rice.aov)
pause()

summary.lm(rice.aov)$coef
pause()

rice$trt <- relevel(rice$trt, ref="NH4Cl")
  # Set NH4Cl as the baseline

fac1 <- factor(sapply(strsplit(as.character(rice$trt)," \\\+"), function(x)x[1]))
anu843 <- sapply(strsplit(as.character(rice$trt), "\\\+"), 
function(x)c("wt","ANU843")[length(x)])
anu843 <- factor(anu843, levels=c("wt", "ANU843"))
attach(rice)
interaction.plot(fac1, anu843, ShootDryMass)
detach(rice)
par(oldpar)

}
\keyword{datasets}
