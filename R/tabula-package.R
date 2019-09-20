#' @details
#' \tabular{ll}{
#'  \strong{Package:} \tab tabula \cr
#'  \strong{Type:} \tab Package \cr
#'  \strong{Version:} \tab 1.3.0 \cr
#'  \strong{License:} \tab GPL-3 \cr
#'  \strong{DOI:} \tab \href{https://doi.org/10.5281/zenodo.1489944}{10.5281/zenodo.1489944}
#' }
#' @author
#' \strong{Full list of authors and contributors} (alphabetic order)
#'
#' \tabular{ll}{
#'  Nicolas Frerebeau \tab \emph{Université Bordeaux Montaigne, France} \cr
#'  Jean-Baptiste Fourvel \tab \emph{CNRS, France} \cr
#'  Brice Lebrun \tab \emph{Aix-Marseille Université, France} \cr
#'  Ben Marwick \tab \emph{University of Washington, USA} \cr
#'  Matthew Peeples \tab \emph{Arizona State University, USA} \cr
#'  Anne Philippe \tab \emph{Université de Nantes, France} \cr
#' }
#'
#' \strong{Package maintainer}
#'
#' Nicolas Frerebeau\cr
#' \email{nicolas.frerebeau@@u-bordeaux-montaigne.fr}
#'
#' IRAMAT-CRP2A (UMR 5060)\cr
#' Maison de l'Archéologie\cr
#' Université Bordeaux Montaigne\cr
#' F-33607 Pessac cedex\cr
#' France
#' @note
#' This work received a state financial support managed by the Agence Nationale
#' de la Recherche (France) through the program \emph{Investissements d'avenir}
#' (ref. ANR-10-LABX-52).
#' @name tabula-package
#' @aliases tabula
#' @docType package
#' @keywords internal
"_PACKAGE"

# Quiets concerns of R CMD check: the .'s that appear in pipelines
# See https://github.com/tidyverse/magrittr/issues/29
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))

#' @importFrom methods new setClass setClassUnion setGeneric setMethod
#' @importFrom rlang .data
NULL
