\name{panel.corr}
\alias{panel.corr}
\title{Scatterplot Panel}
\description{
This function produces a bivariate scatterplot with the Pearson 
correlation.  This is for use with the function \code{panelplot}.
}
\usage{
panel.corr(data, ...)
}
\arguments{
  \item{data}{A data frame with columns x and y}
  \item{...}{Additional arguments} 
}
\author{J.H. Maindonald}

\examples{

# correlation between body and brain weights for 20 mice:

data(litters)
weights <- litters[,-1]
names(weights) <-  c("x","y")
weights <- list(weights)
weights[[1]]$xlim <- range(litters[,2])
weights[[1]]$ylim <- range(litters[,3])
panelplot(weights, panel.corr, totrows=1, totcols=1)
}
\keyword{models}
