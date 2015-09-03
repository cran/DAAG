\name{hurricNamed}
\alias{hurricNamed}
\docType{data}
\title{
Named US Atlantic Hurricanes
}
\description{
  Details are given of atmospheric pressure at landfall, estimated
  damage in millions of dollars, and deaths, for named hurricanes
  that made landfall in the US mainland from 1950 through to 2012.
}
\usage{data("hurricNamed")}
\format{
  A data frame with 94 observations on the following 11 variables.
  \describe{
    \item{\code{Name}}{Hurricane name}
    \item{\code{Year}}{Numeric}
    \item{\code{LF.WindsMPH}}{Maximum sustained windspeed (>= 1 minute) to occur along
    the US coast. Prior to 1980, this is estimated from the maximum
    windspeed associated with the Saffir-Simpson index at landfall. If 2
  or more landfalls, the maximum is taken}
    \item{\code{LF.PressureMB}}{Atmospheric pressure at landfall in
      millibars. If 2 or more landfalls, the minimum is taken}
    \item{\code{LF.times}}{Date of first landfall}
    \item{\code{BaseDam2014}}{Property damage (millions of 2014 US dollars)}
    \item{\code{BaseDamage}}{Property damage (in millions of dollars for that year)}
    \item{\code{NDAM2014}}{Damage, had hurricane appeared in 2014}
    \item{\code{AffectedStates}}{Affected states (2-digit
      abbreviations), pasted together}
    \item{\code{firstLF}}{Date of first landfall}
    \item{\code{deaths}}{Number of continental US direct and indirect deaths}
    \item{\code{mf}}{Gender of name; a factor with levels \code{f} \code{m}}
  }
}
\details{
  An earlier version of these data was the subject of a controversial
  paper that claimed to have found that hurricanes with female names,
  presumably because taken less seriously, did more human damage after
  adjusting for the severity of the storm than those with male names.
}
\source{
  \url{http://www.icatdamageestimator.com/}
  Deaths except for Audrey and Katrina, are in the Excel file
  that is available from
  \url{http://www.pnas.org/content/suppl/2014/05/30/1402786111.DCSupplemental}
  NOAA Monthly Weather Reports (MWRs) supplied the numbers of deaths for
  all except Donna, Celia, Audrey and Katrina.  The figure for Celia is
  from \url{http://www.nhc.noaa.gov/pdf/NWS-TPC-5.pdf}.  For the other
  three hurricanes it is from the Atlantic hurricane list in Wikipedia
  (see the references.)
}
\references{
  \url{http://www.icatdamageestimator.com/}
  \url{www.aoml.noaa.gov/hrd/hurdat/mwr_pdf/}
  \url{http://en.wikipedia.org/wiki/List_of_Atlantic_hurricanes}
  \url{http://www.pnas.org/cgi/doi/10.1073/pnas.1402786111}
}
\examples{
data(hurricNamed)
str(hurricNamed)
plot(log(deaths+0.5) ~ log(NDAM2014), data=hurricNamed)
with(hurricNamed, lines(lowess(log(deaths+0.5) ~ log(NDAM2014))))
plot(log(deaths+0.5) ~ I(NDAM2014^0.14), data=hurricNamed)
with(hurricNamed, lines(lowess(log(deaths+0.1) ~ I(NDAM2014^0.14))))
}
\keyword{datasets}
