\name{moths}
\alias{moths}
\non_function{}
\title{Moths Data}
\usage{data(moths)}
\description{
The \code{moths} data frame has 41 rows and 4 columns.
These data are from a study of the effect of habitat on the
densities of two species of moth (A and P).  Transects were
set across the search area.  Within transects, sections
were identified according to habitat type.  
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{meters}{length of transect}
    \item{A}{number of type A moths found}
    \item{P}{number of type P moths found}
    \item{habitat}{a factor with levels
\code{Bank},
\code{Disturbed},
\code{Lowerside},
\code{NEsoak},
\code{NWsoak},
\code{SEsoak},
\code{SWsoak},
\code{Upperside} }
  }
}
\source{
Sharyn Wragg, formerly of Australian National University
}
\examples{
print("Quasi Poisson Regression - Example 8.3")
data(moths)
rbind(table(moths[,4]), sapply(split(moths[,-4], moths$habitat), apply,2, 
sum))
A.glm <- glm(formula = A ~ log(meters) + factor(habitat), family = 
quasipoisson, data = moths)
summary(A.glm)
moths$habitat <- relevel(moths$habitat, ref="Lowerside")
A.glm <- glm(A ~ habitat + log(meters), family=quasipoisson, data=moths)
summary(A.glm)$coef
}
\keyword{datasets}
