#' @details
#' \tabular{ll}{
#'  \strong{Package:} \tab tabula \cr
#'  \strong{Type:} \tab Package \cr
#'  \strong{Version:} \tab 1.5.1 \cr
#'  \strong{License:} \tab GPL-3 \cr
#'  \strong{DOI:} \tab \href{https://doi.org/10.5281/zenodo.1489944}{10.5281/zenodo.1489944} \cr
#'  \strong{JOSS:} \tab \href{https://doi.org/10.21105/joss.01821}{10.21105/joss.01821} \cr
#' }
#' @author
#' \strong{Full list of authors and contributors} (alphabetic order)
#'
#' \tabular{ll}{
#'  Jean-Baptiste Fourvel \tab \emph{CNRS, France} \cr
#'  Nicolas Frerebeau \tab \emph{Université Bordeaux Montaigne, France} \cr
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
#' @name tabula-package
#' @aliases tabula
#' @docType package
#' @keywords internal
"_PACKAGE"

# Quiets concerns of R CMD check: the .'s that appear in pipelines
# See https://github.com/tidyverse/magrittr/issues/29
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))

#' @import arkhe
#' @importFrom ca ca
#' @importFrom ggplot2 ggplot aes coord_fixed element_blank element_rect
#' element_text facet_wrap geom_line geom_path geom_point geom_tile labs
#' scale_size_area scale_x_continuous scale_x_discrete scale_x_log10
#' scale_y_continuous scale_y_discrete scale_y_log10 theme
#' @importFrom methods as callNextMethod new setClass setClassUnion setGeneric
#' setMethod validObject
#' @importFrom rlang .data
NULL
