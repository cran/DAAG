\name{sorption}
\alias{sorption}
\docType{data}
\title{sorption data set}
\description{
Concentration-time measurements on different varieties of apples
under methyl bromide injection.
}
\usage{data(sorption)}
\format{
  A data frame with 192 observations on the following 14 variables.
  \describe{
    \item{m5}{a numeric vector}
    \item{m10}{a numeric vector}
    \item{m30}{a numeric vector}
    \item{m60}{a numeric vector}
    \item{m90}{a numeric vector}
    \item{m120}{a numeric vector}
    \item{ct}{concentration-time}
    \item{Cultivar}{a factor with levels \code{Pacific Rose} \code{BRAEBURN} \code{Fuji} \code{GRANNY} \code{Gala} \code{ROYAL} \code{Red Delicious} \code{Splendour}}
    \item{Dose}{injected dose of methyl bromide}
    \item{rep}{replicate number, within Cultivar and year}
    \item{year}{a factor with levels \code{1988} \code{1989} \code{1998} \code{1999}}
    \item{year.rep}{a factor with levels \code{1988:1} \code{1988:2} \code{1988:3} \code{1989:1} \code{1989:2} \code{1998:1} \code{1998:2} \code{1998:3} \code{1999:1} \code{1999:2}}
    \item{gp}{a factor with levels \code{BRAEBURN1} \code{BRAEBURN2} \code{Fuji1} \code{Fuji10} \code{Fuji2} \code{Fuji6} \code{Fuji7} \code{Fuji8} \code{Fuji9} \code{GRANNY1} \code{GRANNY2} \code{Gala4} \code{Gala5} \code{Pacific Rose10} \code{Pacific Rose6} \code{Pacific Rose7} \code{Pacific Rose8} \code{Pacific Rose9} \code{ROYAL1} \code{ROYAL2} \code{Red Del10} \code{Red Del9} \code{Red Delicious1} \code{Red Delicious2} \code{Red Delicious3} \code{Red Delicious4} \code{Red Delicious5} \code{Red Delicious6} \code{Red Delicious7} \code{Red Delicious8} \code{Splendour4} \code{Splendour5}}
    \item{inyear}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6}}
  }
}
\keyword{datasets}
