#' Analysis, Seriation and Visualization of Archaeological Count Data
#'
#' An easy way to examine archaeological count data (artifacts, faunal remains,
#' etc.). This package includes several measures of diversity, e.g. richness,
#' rarefaction, diversity, turnover, similarity, etc. It also provides matrix
#' seriation methods for chronological modeling and dating. The package make it
#' easy to visualize count data and statistical thresholds: rank/abundance
#' plots, Ford and Bertin diagrams, etc.
#'
#' \tabular{ll}{
#'  \strong{Package:} \tab tabula \cr
#'  \strong{Type:} \tab Package \cr
#'  \strong{Version:} \tab 1.0.0 \cr
#'  \strong{Date:} \tab 2018-11-22 \cr
#'  \strong{License:} \tab GPL-3 \cr
#'  \strong{DOI:} \tab \href{https://doi.org/10.5281/zenodo.1489944}{10.5281/zenodo.1489944}
#' }
#'
#' @name tabula-package
#' @aliases tabula-package tabula
#' @docType package
#' @author \strong{Full list of authors and contributors} (alphabetic order)
#'  \tabular{ll}{
#'   Nicolas Frerebeau \tab \emph{IRAMAT-CRP2A, Université Bordeaux Montaigne, France} \cr
#'   Brice Lebrun \tab \emph{CEREGE, Aix-Marseille Université, France} \cr
#'   Matthew Peeples \tab \emph{SNArchy, Arizona State University, USA}
#'  }
#'
#' \strong{Bug reporting}
#'
#' - \url{https://github.com/nfrerebeau/tabula/issues}
#'
#' \strong{Package maintainer}
#'
#' Nicolas Frerebea, IRAMAT-CRP2A, Universite Bordeaux Montaigne, Pessac, France,\cr
#' \email{nicolas.frerebeau@@u-bordeaux-montaigne.fr}
#'
#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @import ggplot2
NULL

# Quiets concerns of R CMD check: the .'s that appear in pipelines
# See https://github.com/tidyverse/magrittr/issues/29
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))
