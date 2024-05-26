\name{sumry.glm}
\alias{sumry.glm}
\alias{print.sumry.glm}
\title{Summarizing Generalized Linear Model Fits}
\usage{
\method{sumry}{glm}(object, dispersion = NULL, correlation = FALSE,
        symbolic.cor = FALSE, \dots)

\method{print}{sumry.glm}(x, digits = max(3L, getOption("digits") - 3L), 
                          symbolic.cor = FALSE, 
                          signif.stars = getOption("show.signif.stars"), 
                          call=FALSE, deviance.residuals=FALSE, 
                          show.iter=10, ...) 
}
\arguments{
  \item{object}{an object of class \code{"glm"}, usually, a result of a
    call to \code{\link{glm}}.}
  \item{x}{an object of class \code{"summary.glm"}, usually, a result of a
    call to \code{summary.glm}.}
  \item{dispersion}{the dispersion parameter for the family used.
    Either a single numerical value or \code{NULL} (the default), when
    it is inferred from \code{object} (see \sQuote{Details}).}
  \item{correlation}{logical; if \code{TRUE}, the correlation matrix of
    the estimated parameters is returned and printed.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{symbolic.cor}{logical. If \code{TRUE}, print the correlations in
    a symbolic form (see \code{\link{symnum}}) rather than as numbers.}
  \item{signif.stars}{logical. If \code{TRUE}, \sQuote{significance stars}
    are printed for each coefficient.}
  \item{call}{logical. If \code{TRUE}, details of the function call
    are printed.}  
  \item{deviance.residuals}{logical. If \code{TRUE}, deviance residuals
    are printed.}      
  \item{show.iter}{\code{NULL} or \code{integer}. If \code{NULL}, or if
  the number of iterations is greater than the specified integer,
  then the number of iterations will be printed.}    
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  These functions are \code{\link{methods}} for class \code{glm} or
  \code{sumry.glm} objects.
}
\details{
The function \code{print.sumry.glm} allows, relative to 
\code{print.summary.glm}, some greater flexibility in what is
printed.  By default, details of the call to \code{glm} are omitted,
and details of the number of interations only in the unusual case
where this number is greater than 10.  See the help page for 
\code{\link{summary.glm}} for further details.
}
\value{
  \code{sumry.glm} returns an object of class \code{"sumry.glm"}, a
  list with the same components as \code{summary.glm}.
}
\seealso{
  \code{\link{glm}}, \code{\link{summary}}.
}
\examples{
## For examples see example(glm)
}
\keyword{models}
\keyword{regression}
