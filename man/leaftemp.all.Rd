\name{leaftemp.all}
\alias{leaftemp.all}
\non_function{}
\title{Full Leaf and Air Temperature Data Set}
\usage{data(leaftemp.all)}
\description{
The \code{leaftemp.all} data frame has 62 rows and 10 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{glasshouse}{a factor with levels
\code{A},
\code{B},
\code{C} }
    \item{CO2level}{Carbon Dioxide Levels:
\code{high},
\code{low},
\code{medium} }
    \item{day}{a factor with levels
\code{0},
\code{1},
\code{10},
\code{11},
\code{12},
\code{13},
\code{14},
\code{15},
\code{16},
\code{17},
\code{18},
\code{19},
\code{2}, 
\code{20},
\code{21},
\code{3}, 
\code{4}, 
\code{5},
\code{6}, 
\code{8}, 
\code{9} }
    \item{light}{a numeric vector}
    \item{CO2}{a numeric vector}
    \item{tempDiff}{Difference between Leaf and Air Temperature}
    \item{BtempDiff}{a numeric vector}
    \item{airTemp}{Air Temperature}
    \item{vapPress}{Vapour Pressure}
  }
}
\source{
J.H. Maindonald
}
\examples{
data(leaftemp.all)
}
\keyword{datasets}
