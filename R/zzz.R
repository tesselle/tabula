.onAttach <- function(libname, pkgname){
  options("pboptions" = list(
    type = if (interactive()) "timer" else "none",
    char = "=",
    txt.width = 50,
    gui.width = 300,
    style = 3,
    initial = 0,
    title = "R progress bar",
    label = "",
    nout = 100L,
    min_time = 0))
  invisible(NULL)
}
