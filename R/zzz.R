.onLoad <- function(libname, pkgname){
  op <- options()
  op.tabula <- list(
    tabula.progress = interactive(),
    tabula.verbose = interactive()
  )
  toset <- !(names(op.tabula) %in% names(op))
  if(any(toset)) options(op.tabula[toset])

  invisible()
}
