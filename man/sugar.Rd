\name{sugar}
\alias{sugar}
\non_function{}
\title{Sugar Data}
\usage{data(sugar)}
\description{
The \code{sugar} data frame has 12 rows and 2 columns.
They are from an experiment that
compared an unmodified wild type plant with three different
genetically modified forms.  The measurements are
weights  of sugar that were obtained by breaking down the
cellulose.  
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{weight}{weight, in mg}
    \item{trt}{a factor with levels
\code{Control} i.e. unmodified Wild form,  
\code{A} Modified 1,
\code{B} Modified 2,
\code{C} Modified 3}
  }
}
\source{
Anonymous
}
\examples{
data(sugar)
sugar.aov <- aov(weight ~ trt, data=sugar)
fitted.values(sugar.aov)
summary.lm(sugar.aov)
sugar.aov <- aov(formula = weight ~ trt, data = sugar)
summary.lm(sugar.aov)
}
\keyword{datasets}
