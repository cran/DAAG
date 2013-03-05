\name{tomato}
\alias{tomato}
\docType{data}
\title{Root weights of tomato plants exposed to 4 different treatments}
\description{
The \code{tomato} data frame has 24 rows and 2 columns.
They are from an experiment that exposed tomato plants
to four different 'nutrients'.
}
\usage{data(tomato)}
\format{
  This data frame contains the following columns:
  \describe{
    \item{weight}{weight, in g}
    \item{trt}{a factor with levels
\code{water only},
\code{conc nutrient},
\code{2-4-D + conc nutrient},
\code{3x conc nutrient}}
  }
}
\source{
  Dr Ron Balham, Victoria University of Wellington NZ, sometime
  in 1971 - 1976.
}
\examples{
tomato.aov <- aov(log(weight) ~ trt, data=tomato)
fitted.values(tomato.aov)
summary.lm(tomato.aov)
tomato.aov <- aov(formula = weight ~ trt, data = tomato)
summary.lm(tomato.aov)
}
\keyword{datasets}
