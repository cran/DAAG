\name{cuckoos}
\alias{cuckoos}
\non_function{}
\title{Cuckoo Eggs Data}
\usage{data(cuckoos)}
\description{
Length and breadth measurements of 120 eggs lain in the nests of six 
different species of host bird.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{length}{the egg lengths in tenths of millimeters}
    \item{breadth}{the egg breadths in tenths of millimeters}
    \item{species}{a factor with levels}
   \code{hedge.sparrow}, 
   \code{meadow.pipit}, 
   \code{pied.wagtail}, 
   \code{robin}, 
   \code{tree.pipit}, 
   \code{wren} }
  } 
} 
  \source{ Latter, O.H. (1902). The eggs of Cuculus canorus. An
Inquiry into the dimensions of the cuckoo's egg and the relation of the
variations to the size of the eggs of the foster-parent, with notes on
coloration, &c. Biometrika i, 164.  
} 
  \references{
 Tippett, L.H.C. 1931: "The Methods of Statistics". Williams & Norgate, 
London.
} 
  \examples{ data(cuckoos) 
print("Strip and Boxplots - Example 2.1.2")

attach(cuckoos)
oldpar <- par(las = 2) # labels at right angle to axis.
stripchart(length ~ species) 
boxplot(split(cuckoos$length, cuckoos$species),
         xlab="Length of egg", horizontal=TRUE)
detach(cuckoos)
par(oldpar)
pause()

print("Summaries - Example 2.2.2")
sapply(split(cuckoos$length, cuckoos$species), sd)
pause()

print("Example 4.1.4")
wren <- split(cuckoos$length, cuckoos$species)$wren
median(wren)
n <- length(wren)
sqrt(pi/2)*sd(wren)/sqrt(n)  # this s.e. computation assumes normality
} 
\keyword{datasets}
