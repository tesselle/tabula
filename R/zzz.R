.onLoad <- function(libname, pkgname){
  op <- options()
  op.tabula <- list(
    tabula.progress = TRUE,
    tabula.verbose = TRUE
  )
  toset <- !(names(op.tabula) %in% names(op))
  if(any(toset)) options(op.tabula[toset])

  invisible()
}
