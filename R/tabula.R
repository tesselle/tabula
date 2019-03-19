#' @details
#' \tabular{ll}{
#'  \strong{Package:} \tab tabula \cr
#'  \strong{Type:} \tab Package \cr
#'  \strong{Version:} \tab 1.2.0 \cr
#'  \strong{Date:} \tab 2019-03-19 \cr
#'  \strong{License:} \tab GPL-3 \cr
#'  \strong{DOI:} \tab \href{https://doi.org/10.5281/zenodo.1489944}{10.5281/zenodo.1489944}
#' }
#' @author
#' \strong{Full list of authors and contributors} (alphabetic order)
#'
#' \tabular{ll}{
#'  Nicolas Frerebeau \tab \emph{IRAMAT-CRP2A, Université Bordeaux Montaigne, France} \cr
#'  Brice Lebrun \tab \emph{CEREGE, Aix-Marseille Université, France} \cr
#'  Matthew Peeples \tab \emph{SNArchy, Arizona State University, USA}
#' }
#'
#' \strong{Package maintainer}
#'
#' Nicolas Frerebeau\cr
#' \email{nicolas.frerebeau@@u-bordeaux-montaigne.fr}
#'
#' IRAMAT-CRP2A (UMR 5060)\cr
#' Maison de l'Archéologie\cr
#' Universite Bordeaux Montaigne\cr
#' F-33607 Pessac cedex\cr
#' France
#' @note
#' This work received a state financial support managed by the Agence Nationale
#' de la Recherche (France) throught the program \emph{Investissements d'avenir}
#' (ref. ANR-10-LABX-52).
#' @name tabula-package
#' @aliases tabula-package tabula
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @import dplyr
#' @import ggplot2
#' @import tidyr
NULL

# Quiets concerns of R CMD check: the .'s that appear in pipelines
# See https://github.com/tidyverse/magrittr/issues/29
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))
