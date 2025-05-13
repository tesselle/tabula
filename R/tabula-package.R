#' @details
#' \tabular{ll}{
#'  **Version** \tab 3.3.0 \cr
#'  **License** \tab GPL-3 \cr
#'  **CRAN DOI** \tab \doi{10.32614/CRAN.package.tabula} \cr
#'  **Zenodo DOI** \tab \doi{10.5281/zenodo.1489944} \cr
#'  **JOSS DOI** \tab \doi{10.21105/joss.01821} \cr
#' }
#'
#' Archéosciences Bordeaux (UMR 6034)\cr
#' Maison de l'Archéologie\cr
#' Université Bordeaux Montaigne\cr
#' 33607 Pessac cedex\cr
#' France
#'
#' @section Package options:
#'  \pkg{tabula} uses the following [options()] to configure behavior:
#'  * `tabula.progress`: a [`logical`] scalar. Should progress bars be
#'    displayed? Defaults to [interactive()].
#'  * `tabula.verbose`: a [`logical`] scalar. Should \R report extra information
#'    on progress? Defaults to [interactive()].
#'
#' @name tabula-package
#' @aliases tabula
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @import arkhe
#' @importFrom methods as callGeneric initialize is new setClass setOldClass
#' setGeneric setMethod validObject .valueClassTest
#' @importFrom stats as.dist na.omit pairwise.table pt quantile rmultinom sd
#' @importFrom utils combn head modifyList setTxtProgressBar tail txtProgressBar
NULL
