\name{head.injury}
\alias{head.injury}
\non_function{}
\title{Minor Head Injury (Simulated) Data}
\usage{data(head.injury)}
\description{
The \code{head.injury} data frame has 3121 rows and 11 columns.
The data were simulated according to a simple logistic
regression model to match roughly the clinical characteristics
of a sample of individuals who suffered minor head injuries.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{age.65}{age factor (0 = under 65, 1 = over 65).}
    \item{amnesia.before}{amnesia before impact (less than 30 minutes = 0, 
more 
than 30 minutes =1).}
    \item{basal.skull.fracture}{(0 = no fracture, 1 = fracture).}
    \item{GCS.decrease}{Glasgow Coma Scale decrease (0 = no 
deterioration, 1 = deterioration).}
    \item{GCS.13}{initial Glasgow Coma Scale (0 = not `13', 1 = `13'). }
    \item{GCS.15.2hours}{Glasgow Coma Scale after 2 hours
(0 = not `15', 1 = '15').}
    \item{high.risk}{assessed by clinician as high risk for neurological
intervention (0 = not high risk, 1 = high risk).}
    \item{loss.of.consciousness}{(0 = conscious, 1 = loss of 
consciousness).}
    \item{open.skull.fracture}{(0 = no fracture, 1 = fracture) }
    \item{vomiting}{(0 = no vomiting, 1 = vomiting)}
    \item{clinically.important.brain.injury}{any acute brain finding
revealed on CT (0 = not present, 1 = present).}
  }
}
\references{
Stiell, I.G., Wells, G.A., Vandemheen, K., Clement, C., Lesiuk, H.,
Laupacis, A., McKnight, R.D., Verbee, R., Brison, R., Cass, D., 
Eisenhauer, M., Greenberg, G.H., and Worthington, J. (2001) 
The Canadian CT Head Rule for Patients with Minor Head Injury,
The Lancet. 357: 1391-1396. 
}
\examples{
data(head.injury)
## maybe str(head.injury) ; plot(head.injury) ...
}
\keyword{datasets}
