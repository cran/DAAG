\name{plotSimScat}
\alias{plotSimScat}
\title{
Simulate scatterplots, from \code{lm} object with a single explanatory variable.
}
\description{
This plots simulated y-values, or residuals from such simulations,
against x-values
.}
\usage{
plotSimScat(obj, sigma = NULL, layout = c(4, 1), type = c("p", "r"),
show = c("points", "residuals"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
An \code{lm} object with a single explanatory variable.
}
  \item{sigma}{
    Standard deviation, if different from that for the supplied \code{lm}
    object.
}
  \item{layout}{
Columns by Rows layout for plots from the simulations.
}
  \item{type}{
See \code{type} as in \code{\link{plot.lm}}.
}
  \item{show}{
Specify \code{points} or {residuals}.
}
  \item{...}{Other parameters to be passed to plotting functions}
}
\value{
A lattice graphics object is returned.
}

\author{
J H Maindonald
}

\seealso{
\code{\link{plotSimDiags}}
}
\examples{
nihills.lm <- lm(timef~time, data=nihills)
plotSimDiags(nihills.lm)

## The function is currently defined as
function (obj, sigma = NULL, layout = c(4, 1), type = c("p",
    "r"), show = c("points", "residuals"))
{
    nsim <- prod(layout)
    if (is.null(sigma))
        sigma <- summary(obj)[["sigma"]]
    hat <- fitted(obj)
    xnam <- all.vars(formula(obj))[2]
    ynam <- all.vars(formula(obj))[1]
    df <- data.frame(sapply(1:nsim, function(x) rnorm(length(hat),
        sd = sigma)))
    if (show[1] == "points")
        df <- df + hat
    simnam <- names(df) <- paste("Simulation", 1:nsim, sep = "")
    df[, c(xnam, ynam)] <- model.frame(obj)[, c(xnam, ynam)]
    if (show[1] != "points") {
        df[, "Residuals"] <- df[, ynam] - hat
        ynam <- "Residuals"
        legadd <- "residuals"
    }
    else legadd <- "data"
    leg <- list(text = paste(c("Simulated", "Actual"), legadd),
        columns = 2)
    formula <- formula(paste(paste(simnam, collapse = "+"), "~",
        xnam))
    parset <- simpleTheme(pch = c(16, 16), lty = 2, col = c("black",
        "gray"))
    gph <- xyplot(formula, data = df, outer = TRUE, par.settings = parset,
        auto.key = leg, lty = 2, layout = layout, type = type)
    formxy <- formula(paste(ynam, "~", xnam))
    addgph <- xyplot(formxy, data = df, pch = 16, col = "gray")
    gph + as.layer(addgph, under = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{regression}
\keyword{graphics}
