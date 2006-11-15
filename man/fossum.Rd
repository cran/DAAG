\name{fossum}
\alias{fossum}
\non_function{}
\title{Female Possum Measurements}
\usage{fossum}
\description{
The \code{fossum} data frame consists of nine morphometric
measurements on each of 43 female mountain brushtail possums, trapped
at seven sites from Southern Victoria to central Queensland.
This is a subset of the \code{possum} data frame.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{case}{observation number}
    \item{site}{one of seven locations where possums were
trapped}
    \item{Pop}{a factor which classifies the sites as
\code{Vic} Victoria,
\code{other} New South Wales or Queensland}
    \item{sex}{a factor with levels
\code{f} female,
\code{m} male }
\item{age}{age}
    \item{hdlngth}{head length}
    \item{skullw}{skull width}
    \item{totlngth}{total length}
    \item{taill}{tail length}
    \item{footlgth}{foot length}
    \item{earconch}{ear conch length}
    \item{eye}{distance from medial canthus to lateral canthus of
right eye}
    \item{chest}{chest girth (in cm)}
    \item{belly}{belly girth (in cm)}
  }
}
\source{
Lindenmayer, D. B., Viggers, K. L., Cunningham, R. B., and
Donnelly, C. F. 1995. Morphological variation among columns of the
mountain brushtail possum, Trichosurus caninus Ogilby
(Phalangeridae: Marsupiala). Australian Journal of Zoology 43:
449-458. 
}
\examples{
boxplot(fossum$totlngth)
}
\keyword{datasets}
