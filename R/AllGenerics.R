#' @include AllClasses.R
NULL

# ==============================================================================
#' \eqn{\alpha}{\alpha}-diversity
#'
#' @description
#' Measures within sample diversity. \code{diversity} returns a diversity or
#' dominance index. \code{evenness} returns an evenness measure.
#' @param object A \eqn{m \times p}{m x p} matrix.
#' @param method A \code{\link{character}} string specifiying the index to be
#'  computed. This must be one or more of "\code{berger}",
#'  "\code{brillouin}", "\code{margalef}", "\code{mcintosh}",
#'  "\code{menhinick}", "\code{shannon}", "\code{simpson}" (see details).
#'  Any unambiguous substring can be given.
#' @param simplify A \code{\link{logical}} scalar: should the result be
#'  simplified to a matrix? The default value, \code{FALSE}, returns a list.
#' @param ... Further arguments passed to other methods.
#' @details
#'  \emph{Diversity} measurement assumes that all individuals in a specific
#'  taxon are equivalent and that all types are equally different from each
#'  other (Peet 1974). A measure of diversity can be achieved by using indices
#'  built on the relative abundance of taxa. These indices (sometimes referred
#'  to as non-parametric indices) benefit from not making assumptions about the
#'  underlying distribution of taxa abundance: they only take evenness and
#'  \link[=richness-method]{richness} into account. Peet (1974) refers to them
#'  as indices of \emph{heterogeneity}.
#'
#'  Diversity index focus on one aspect of the taxa abundance and emphasize
#'  either \link[=richness-method]{richness} (weighting towards uncommon taxe)
#'  or dominance (weighting towards abundant taxa; Magurran 1988).
#'
#'  \emph{Evenness} is a measure of how evenly individuals are distributed
#'  across the sample.
#'
#'  The following heterogeneity index and corresponding evenness measures
#'  are available (see Magurran 1988 for details):
#'  \describe{
#'   \item{berger}{Berger-Parker dominance index. The Berger-Parker index
#'    expresses the proportional importance of the most abundant type.}
#'   \item{brillouin}{Brillouin diversity index. The Brillouin index describes a
#'    known collection: it does not assume random sampling in an infinite
#'    population. Pielou (1975) and Laxton (1978) argues for the use of the
#'    Brillouin index in all circumstances, especially in preference to the
#'    Shannon index.}
#'   \item{mcintosh}{McIntosh dominance index. The McIntosh index expresses the
#'    heterogeneity of a sample in geometric terms. It describes the sample as a
#'    point of a S-dimensional hypervolume and uses the Euclidean distance of
#'    this point from the origin.}
#'   \item{shannon}{Shannon-Wiener diversity index. The Shannon index assumes
#'    that individuals are randomly sampled from an infinite population and that
#'    all types are represented in the sample. This index does not reflect the
#'    sample size.}
#'   \item{simpson}{Simpson dominance index. The Simpson index expresses the
#'    probability that two individuals randomly picked from a finite sample
#'    belong to two different types. It can be interpreted as the weighted mean
#'    of the proportional abundances. The form for a finite sample is used
#'    here.}
#'  }
#'
#'  The \code{berger}, \code{mcintosh} and \code{simpson} methods return a
#'  \emph{dominance} index, not the reciprocal form usually adopted, so that an
#'  increase in the value of the index accompanies a decrease in diversity.
#' @return
#'  If \code{simplify} is \code{FALSE} returns a list (default), else
#'  returns a matrix.
#' @references
#'  Berger, W. H. & Parker, F. L. (1970). Diversity of Planktonic Foraminifera
#'  in Deep-Sea Sediments. \emph{Science}, 168(3937), 1345-1347.
#'  DOI: \href{https://doi.org/10.1126/science.168.3937.1345}{10.1126/science.168.3937.1345}.
#'
#'  Magurran, A. E. (1988). \emph{Ecological Diversity and its Measurement}.
#'  Princeton, NJ: Princeton University Press.
#'  DOI:\href{https://doi.org/10.1007/978-94-015-7358-0}{10.1007/978-94-015-7358-0}.
#'
#'  McIntosh, R. P. (1967). An Index of Diversity and the Relation of Certain
#'  Concepts to Diversity. \emph{Ecology}, 48(3), 392-404.
#'  DOI: \href{https://doi.org/10.2307/1932674}{10.2307/1932674}.
#'
#'  Peet, R. K. (1974). The Measurement of Species Diversity. \emph{Annual
#'  Review of Ecology and Systematics}, 5(1), 285-307.
#'  DOI: \href{https://doi.org/10.1146/annurev.es.05.110174.001441}{10.1146/annurev.es.05.110174.001441}.
#'
#'  Pielou, E. C. (1975). \emph{Ecological Diversity}. New York: Wiley.
#'  DOI: \href{https://doi.org/10.4319/lo.1977.22.1.0174b}{10.4319/lo.1977.22.1.0174b}
#'
#'  Shannon, C. E. (1948). A Mathematical Theory of Communication. \emph{The
#'  Bell System Technical Journal}, 27, 379-423.
#'  DOI: \href{https://doi.org/10.1002/j.1538-7305.1948.tb01338.x}{10.1002/j.1538-7305.1948.tb01338.x}.
#'
#'  Simpson, E. H. (1949). Measurement of Diversity. \emph{Nature}, 163(4148),
#'  688-688. DOI: \href{https://doi.org/10.1038/163688a0}{10.1038/163688a0}.
#' @example inst/examples/ex-alpha.R
#' @author N. Frerebeau
#' @seealso
#'  \code{\link[=richness-method]{richness}}
#'  \code{\link[=richness-method]{rarefaction}}
#'  \code{\link[=beta-diversity]{turnover}}
#'  \code{\link[=beta-diversity]{similarity}}
#' @docType methods
#' @name alpha-diversity
#' @rdname alpha-diversity
NULL

#' @rdname alpha-diversity
#' @aliases diversity-method
setGeneric(
  name = "diversity",
  def = function(object, ...) standardGeneric("diversity")
)

#' @rdname alpha-diversity
#' @aliases evenness-method
setGeneric(
  name = "evenness",
  def = function(object, ...) standardGeneric("evenness")
)

# ==============================================================================
#' \eqn{\beta}{\beta}-diversity
#'
#' @description
#'  Measures between samples diversity. \code{similarity} returns a similarity
#'  matrix. \code{turnover} returns the degree of turnover in taxa composition
#'  along a grandient or transect.
#' @param object A \eqn{m \times p}{m x p} matrix.
#' @param method A \code{\link{character}} string specifiying the method to be
#'  used. This must be one of "\code{jaccard}", "\code{sorenson}",
#'  "\code{bray}" or "\code{morisita}" (see details).
#'  Any unambiguous substring can be given.
#' @param simplify A \code{\link{logical}} scalar: should the result be
#'  simplified to a matrix? The default value, \code{FALSE}, returns a list.
#' @param ... Further arguments passed to other methods.
#' @details
#'  similarity Entre 0 et 1.
#'  turnover Begining of the transect in the first row
#'  \describe{
#'   \item{bray}{Sorenson quantitative index (Bray and Curtis modified version
#'   of the Sorenson index). }
#'   \item{jaccard}{Jaccard qualitative index.}
#'   \item{morisita}{Morisita-Horn quantitative index.}
#'   \item{sorenson}{Sorenson qualitative index.}
#'  }
#' @return
#'  \code{similarity} returns a \eqn{m \times \m}{m x m} symetric matrix.
#'  If \code{simplify} is \code{FALSE}, \code{turnover} returns a list
#'  (default), else returns a matrix.
#' @example inst/examples/ex-beta.R
#' @author N. Frerebeau
#' @seealso
#'  \code{\link[=richness-method]{richness}}
#'  \code{\link[=richness-method]{rarefaction}}
#'  \code{\link[=alpha-diversity]{diversity}}
#'  \code{\link[=alpha-diversity]{evenness}}
#' @docType methods
#' @name beta-diversity
#' @rdname beta-diversity
NULL

#' @rdname beta-diversity
#' @aliases turnover-method
setGeneric(
  name = "turnover",
  def = function(object, ...) standardGeneric("turnover")
)

#' @rdname beta-diversity
#' @aliases similarity-method
setGeneric(
  name = "similarity",
  def = function(object, ...) standardGeneric("similarity")
)

# ==============================================================================
#' Permute rows or columns
#'
#' @param object An object.
#' @param ... Further arguments passed to other methods.
#' @author N. Frerebeau
#' @seealso \code{\link{seriate}}
#' @export
#' @docType methods
#' @rdname permute-method
setGeneric(
  name = "permute",
  def = function(object, ...) standardGeneric("permute")
)

# Plot =========================================================================
#' Bar plot
#'
#' XXX
#' @param object An object to be plotted.
#' @param EPPM A \code{\link{logical}} scalar: should TODO?.
#' @param center A \code{\link{logical}} scalar: should the bar plot
#'  be centered? The default value, \code{TRUE}, produces a Ford diagram.
#' @param horizontal A \code{\link{logical}} scalar: should the bar plot
#'  be horizontal? The default value, \code{FALSE}, means variables in rows and
#'  cases in columns (i.e. Bertin diagram). Only used if \code{center} is
#'  \code{FALSE}.
#' @param ... Further arguments passed to other methods.
#' @details
#'  TODO
#'  coerce to frequency matrix first
#' @references
#'  Desachy, B. (2004). Le sériographe EPPM: un outil informatisé de sériation
#'  graphique pour tableaux de comptages. \emph{Revue archéologique de
#'  Picardie}, 3(1), 39-56.
#'  DOI: \href{https://doi.org/10.3406/pica.2004.2396}{10.3406/pica.2004.2396}.
#' @example inst/examples/ex-plotBar.R
#' @author N. Frerebeau
#' @family plot
#' @docType methods
#' @rdname plotBar-method
#' @aliases plotBar-method
setGeneric(
  name = "plotBar",
  def = function(object, ...) standardGeneric("plotBar")
)
# ------------------------------------------------------------------------------
#' Matrix plot
#'
#' XXX
#' @param object An object to be plotted.
#' @param PVI A \code{\link{logical}} scalar: should TODO?
#' @param center A \code{\link{logical}} scalar: should TODO?
#' @param ... Further arguments passed to other methods.
#' @details
#'  TODO
#'  coerce to count matrix first
#' @references
#'  Desachy, B. (2004). Le sériographe EPPM: un outil informatisé de sériation
#'  graphique pour tableaux de comptages. \emph{Revue archéologique de
#'  Picardie}, 3(1), 39-56.
#'  DOI: \href{https://doi.org/10.3406/pica.2004.2396}{10.3406/pica.2004.2396}.
#' @example inst/examples/ex-plotMatrix.R
#' @author N. Frerebeau
#' @family plot
#' @docType methods
#' @rdname plotMatrix-method
#' @aliases plotMatrix-method
setGeneric(
  name = "plotMatrix",
  def = function(object, ...) standardGeneric("plotMatrix")
)
# ------------------------------------------------------------------------------
#' Rank vs abundance plot
#'
#' @param object An object to be plotted.
#' @param log A \code{\link{character}} string which contains "\code{x}" if the
#' x axis is to be logarithmic, "\code{y}" if the y axis is to be logarithmic
#' and "\code{xy}" or "\code{yx}" if both axes are to be logarithmic.
#' @param facet A \code{\link{logical}} scalar: should TODO?
#' @param ... Further arguments passed to other methods.
#' @details
#' variables scaled to 0-1 (frequency)
#' @example inst/examples/ex-plotRank.R
#' @author N. Frerebeau
#' @family plot
#' @docType methods
#' @rdname plotRank-method
#' @aliases plotRank-method
setGeneric(
  name = "plotRank",
  def = function(object, ...) standardGeneric("plotRank")
)
# ------------------------------------------------------------------------------
#' Spot plot
#'
#' XXX
#' @param object An object to be plotted.
#' @param threshold A \code{\link{character}} string specifiying the threshold
#'  to be plotted. This must be one or more of "\code{mean}" or "\code{median}".
#'  Any unambiguous substring can be given.
#' @param ... Further arguments passed to other methods.
#' @details
#'  TODO
#' variables scaled to 0-1 (frequency)
#' @note The spot plot is inspired from Dan Gopstein's
#'  \href{https://dgopstein.github.io/articles/spot-matrix/}{spot matrix},
#'  so credit should be given to him.
#' @example inst/examples/ex-plotSpot.R
#' @author N. Frerebeau
#' @family plot
#' @docType methods
#' @rdname plotSpot-method
#' @aliases plotSpot-method
setGeneric(
  name = "plotSpot",
  def = function(object, ...) standardGeneric("plotSpot")
)

# ==============================================================================
#' Richness and rarefaction
#'
#' @description
#'  \code{richness} returns sample richness. \code{rarefaction} returns
#'  Hurlbert's unbiaised estimate of Sander's rarefaction.
#' @param object A \eqn{m \times p}{m x p} \code{\link{numeric}} matrix.
#' @param sample A length-one \code{\link{numeric}} vector giving the sub-sample
#'  size.
#' @param method A \code{\link{character}} string specifiying the index to be
#'  computed. This must be one or more of "\code{margalef}" or
#'  "\code{menhinick}" (see details). Any unambiguous substring can be given.
#' @param simplify A \code{\link{logical}} scalar: should the result be
#'  simplified to a matrix? The default value, \code{FALSE}, returns a list.
#' @param ... Further arguments passed to other methods.
#' @details
#'  The number of different taxa, provides an instantly comprehensible
#'  expression of diversity. While the number of taxa within a sample
#'  is easy to ascertain, as a term, it makes little sense: some taxa
#'  may not have been seen, or there may not be a fixed number of taxa
#'  (e.g. in an open system; Peet 1974). As an alternative, \emph{richness}
#'  (\eqn{S}) can be used for the concept of taxa number (McIntosh 1967).
#'
#'  It is not always possible to ensure that all sample sizes are equal
#'  and the number of different taxa increases with sample size and
#'  sampling effort. Then, \emph{rarefaction} (\eqn{E(S)}) is the number of
#'  taxa expected if all samples were of a standard size (i.e. taxa per fixed
#'  number of individuals). Rarefaction assumes that imbalances between taxa
#'  are due to sampling and not to differences in actual abundances.
#'
#'  The following richness measures are available:
#'  \describe{
#'   \item{margalef}{Margalef richness index:
#'    \eqn{D_{Mg} = \frac{S - 1}{\ln N}}{D = (S - 1) / ln N}}
#'   \item{menhinick}{Menhinick richness index:
#'    \eqn{D_{Mn} = \frac{S}{\sqrt{N}}}{D = S / \sqrt N}}
#'  }
#' @note
#'  Ramanujan approximation is used for \eqn{x!} computation if \eqn{x > 170}.
#' @return A \code{\link{numeric}} vector.
#' @references
#'  Hurlbert, S. H. (1971). The Nonconcept of Species Diversity: A Critique and
#'  Alternative Parameters. \emph{Ecology}, 52(4), 577-586.
#'  DOI: \href{https://doi.org/10.2307/1934145}{10.2307/1934145}.
#'
#'  Margalef, R. (1958). Information Theory in Ecology. \emph{General Systems},
#'  3, 36-71.
#'
#'  McIntosh, R. P. (1967). An Index of Diversity and the Relation of Certain
#'  Concepts to Diversity. \emph{Ecology}, 48(3), 392-404.
#'  DOI: \href{https://doi.org/10.2307/1932674}{10.2307/1932674}.
#'
#'  Menhinick, E. F. (1964). A Comparison of Some Species-Individuals Diversity
#'  Indices Applied to Samples of Field Insects. \emph{Ecology}, 45(4), 859-861.
#'  DOI: \href{https://doi.org/10.2307/1934933}{10.2307/1934933}.
#'
#'  Peet, R. K. (1974). The Measurement of Species Diversity. \emph{Annual
#'  Review of Ecology and Systematics}, 5(1), 285-307.
#'  DOI: \href{https://doi.org/10.1146/annurev.es.05.110174.001441}{10.1146/annurev.es.05.110174.001441}.
#'
#'  Sander, H. L. (1968). Marine Benthic Diversity: A Comparative Study.
#'  \emph{The American Naturalist}, 102(925), 243-282.
#' @seealso
#'  \code{\link[=alpha-diversity]{diversity}}
#'  \code{\link[=alpha-diversity]{evenness}}
#'  \code{\link[=beta-diversity]{turnover}}
#'  \code{\link[=beta-diversity]{similarity}}
#' @author N. Frerebeau
#' @docType methods
#' @name richness-method
#' @rdname richness-method
NULL

#' @rdname richness-method
#' @aliases rarefaction-method
setGeneric(
  name = "rarefaction",
  def = function(object, ...) standardGeneric("rarefaction")
)

#' @rdname richness-method
#' @aliases richness-method
setGeneric(
  name = "richness",
  def = function(object, ...) standardGeneric("richness")
)

# ==============================================================================
# Rescale data
if (!isGeneric("rescale")) {
  setGeneric(
    name = "rescale",
    def = function(object, ...) standardGeneric("rescale")
  )
}

# ==============================================================================
#' Seriate
#'
#' @param object An object.
#' @param ... Further arguments passed to other methods.
#' @author N. Frerebeau
#' @seealso \code{\link{permute}}
#' @export
#' @docType methods
#' @rdname seriate-method
#' @aliases seriate-method
setGeneric(
  name = "seriate",
  def = function(object, ...) standardGeneric("seriate")
)
