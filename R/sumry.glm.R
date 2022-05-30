sumry.glm <-
function (object, dispersion = NULL, correlation = FALSE, symbolic.cor = FALSE, ...) 
{
  ans <- summary.glm(object, dispersion = NULL, correlation = FALSE, symbolic.cor = FALSE, 
                      ...)
  class(ans) <- "sumry.glm"
  return(ans)
}