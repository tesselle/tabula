#' @include AllClasses.R
NULL

# Show =========================================================================
## DateEvent -------------------------------------------------------------------
setMethod(
  f = "show",
  signature = "DateEvent",
  definition = function(object) {
    cat("Modelled event date (years, calendar time, CI: ",
        object@level * 100, "%):", "\n", sep = "")
    print(methods::as(object, "matrix"))
  }
)

# Coerce =======================================================================
## From DateEvent --------------------------------------------------------------
setAs(from = "DateEvent", to = "matrix", def = function(from) {
  event <- cbind(from@earliest, from@date, from@latest)
  dimnames(event) <- list(from@assemblage, c("Earliest", "Estimation", "Latest"))
  return(event)
})
setAs(from = "DateEvent", to = "data.frame", def = function(from)
  as.data.frame(methods::as(from, "matrix"))
)
