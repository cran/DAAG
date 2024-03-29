\name{ais}
\alias{ais}
\docType{data}
\title{Australian athletes data set}
\description{
  These data were collected in a study of how data on 
  various characteristics of the blood varied with sport,
  body size, and sex of the athlete.
}
\usage{data(ais)}
\format{
  A data frame with 202 observations on the following 13 variables.
  \describe{
    \item{rcc}{red blood cell count, in \eqn{10^{12} l^{-1}}}
    \item{wcc}{while blood cell count, in \eqn{10^{12}} per liter}
    \item{hc}{hematocrit, percent}
    \item{hg}{hemaglobin concentration, in g per decaliter}
    \item{ferr}{plasma ferritins, ng \eqn{dl^{-1}}}
    \item{bmi}{Body mass index, kg \eqn{cm^{-2} 10^2}}
    \item{ssf}{sum of skin folds}
    \item{pcBfat}{percent Body fat}
    \item{lbm}{lean body mass, kg}
    \item{ht}{height, cm}
    \item{wt}{weight, kg}
    \item{sex}{a factor with levels \code{f} \code{m}}
    \item{sport}{a factor with levels \code{B_Ball} \code{Field} 
\code{Gym} \code{Netball} \code{Row} \code{Swim} \code{T_400m} 
\code{T_Sprnt} \code{Tennis} \code{W_Polo}}
  }
}
\details{
  Do blood hemoglobin concentrations of athletes in endurance-related
  events differ from those in power-related events?
}
\source{
  These data were the basis for the analyses that are reported in
  Telford and Cunningham (1991).
}
\references{
Telford, R.D. and Cunningham, R.B. 1991.  Sex, sport and
  body-size dependency of hematology in highly trained athletes.
  Medicine and Science in Sports and Exercise 23: 788-794.
}
\keyword{datasets}
