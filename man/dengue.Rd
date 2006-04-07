\name{dengue}
\alias{dengue}
\docType{data}

\title{Dengue prevalence, by administrative region}
\description{Data record, for each of 2000 administrative regions,
whether or not dengue was recorded at any time between 1961 and 1990.
}
\usage{data(dengue)}
\format{
  A data frame with 2000 observations on the following 13 variables.
  \describe{
    \item{humid}{Average vapour density: 1961-1990}
    \item{humid90}{90th percentile of \code{humid}}
    \item{temp}{Average temperature: 1961-1990}
    \item{temp90}{90th percentile of \code{temp}}
    \item{h10pix}{maximum of \texttt{humid}, within a 10 pixel radius}
    \item{h10pix90}{maximum of \texttt{humid90}, within a 10 pixel radius}
    \item{trees}{Percent tree cover, from satellite data}
    \item{trees90}{90th percentile of \texttt{trees}}
    \item{NoYes}{Was dengue observed? (1=yes)}
    \item{Xmin}{minimum longitude}
    \item{Xmax}{maximum longitude}
    \item{Ymin}{minimum latitude}
    \item{Ymax}{maximum latitude}
  }
}
\details{
  This is derived from a data set in which the climate and tree cover
  information were given for each half degree of latitude by half
  degreee of longitude pixel.
  The variable \code{NoYes} was given by administrative region.
  The climate data and tree cover data given here are 50th or 90th
  percentiles, where percetiles were calculates across pixels for an
  administrative region.
}
\source{
Simon Hales, Environmental Research New Zealand Ltd.
}
\references{
Hales, S., de Wet, N., Maindonald, J. and Woodward, A.
  2002.  Potential effect of population and climate change global
  distribution of dengue fever: an empirical model.  The Lancet 2002;
  360: 830-34. 
}
\examples{
str(dengue)
glm(NoYes ~ humid, data=dengue, family=binomial)
glm(NoYes ~ humid90, data=dengue, family=binomial)
}
\keyword{datasets}
