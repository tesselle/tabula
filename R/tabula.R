#' \code{tabula} package
#'
#' Analysis, Seriation and Visualization of Archaeological Count Data
#'
#' See the README on
#' \href{https://github.com/nfrerebeau/tabula#readme}{GitHub}
#'
#' @docType package
#' @name tabula
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @import ggplot2
NULL

# Quiets concerns of R CMD check: the .'s that appear in pipelines
# See https://github.com/tidyverse/magrittr/issues/29
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))
