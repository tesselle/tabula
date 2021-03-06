# GENERIC METHODS
#' @include AllClasses.R
NULL

# S4 dispatch to base S3 generic ===============================================
if (!isGeneric("plot"))
  setGeneric("plot", function(x, y, ...) standardGeneric("plot"))

# Extract ======================================================================
## Mutators --------------------------------------------------------------------
#' Get or Set Parts of an Object
#'
#' Getters and setters to extract or replace parts of an object.
#' @param x An object from which to get or set element(s).
# @param value A possible value for the element(s) of `object` (see below).
#' @return
#'  An object of the same sort as `object` with the new values assigned.
# @example inst/examples/ex-mutator.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name mutator
#' @rdname mutator
#' @aliases get set
NULL

#' @rdname mutator
#' @aliases get_index-method
setGeneric(
  name = "get_index",
  def = function(x) standardGeneric("get_index")
)

#' @rdname mutator
#' @aliases get_method-method
setGeneric(
  name = "get_method",
  def = function(x) standardGeneric("get_method")
)

#' @rdname mutator
#' @aliases get_order-method
setGeneric(
  name = "get_order",
  def = function(x) standardGeneric("get_order")
)

## Subset ----------------------------------------------------------------------
#' Extract or Replace Parts of an Object
#'
#' Operators acting on objects to extract or replace parts.
#' @param x An object from which to extract element(s) or in which to replace
#'  element(s).
#' @param i A [`character`] string specifying elements to extract.
#'  Any unambiguous substring can be given (see details).
#' @return
#'  A subsetted object.
# @example inst/examples/ex-mutator.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutator
#' @name subset
#' @rdname subset
NULL

# Statistic ====================================================================
#' Independance
#'
#' @param object A [`CountMatrix-class`] object.
#' @param ... Currently not used.
#' @details
#'  Computes for each cell of a numeric matrix one of the following statistic.
#' @section EPPM:
#'  This positive difference from the column mean percentage (in french "écart
#'  positif au pourcentage moyen", EPPM) represents a deviation from the
#'  situation of statistical independence. As independence can be interpreted as
#'  the absence of relationships between types and the chronological order of
#'  the assemblages, `EPPM` is a useful graphical tool to explore significance
#'  of relationship between rows and columns related to seriation (Desachy
#'  2004).
#' @section PVI:
#'  `PVI` is calculated for each cell as the percentage to the column
#'  theoretical independence value: `PVI` greater than \eqn{1} represent
#'  positive deviations from the independence, whereas `PVI` smaller than
#'  \eqn{1} represent negative deviations (Desachy 2004).
#'
#'  The `PVI` matrix allows to explore deviations from independence
#'  (an intuitive graphical approach to \eqn{\chi^2}{Chi-squared}),
#'  in such a way that a high-contrast matrix has quite significant deviations,
#'  with a low risk of being due to randomness (Desachy 2004).
#' @references
#'  Desachy, B. (2004). Le sériographe EPPM: un outil informatisé de sériation
#'  graphique pour tableaux de comptages. *Revue archéologique de Picardie*,
#'  3(1), 39-56. \doi{10.3406/pica.2004.2396}.
#' @return A [`numeric`] [`matrix`].
#' @example inst/examples/ex-independance.R
#' @seealso [plot_ford()], [plot_heatmap()], [seriate_rank()]
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @name independance
#' @rdname independance
NULL

#' @rdname independance
#' @aliases eppm-method
setGeneric(
  name = "eppm",
  def = function(object, ...) standardGeneric("eppm")
)

#' @rdname independance
#' @aliases pvi-method
setGeneric(
  name = "pvi",
  def = function(object, ...) standardGeneric("pvi")
)

# Diversity ====================================================================
## Heterogeneity ---------------------------------------------------------------
#' Heterogeneity and Evenness
#'
#' @description
#'  * `index_heterogeneity()` returns an heterogeneity or dominance index.
#'  * `index_evenness()` returns an evenness measure.
#'  * `bootstrap_*()` and `jackknife_*()` perform bootstrap/jackknife
#'    resampling.
#' @param object A \eqn{m \times p}{m x p} matrix of count data (typically
#'  a [`CountMatrix-class`] object).
#' @param method A [`character`] string specifying the index to be computed
#' (see details). Any unambiguous substring can be given.
#' @param quantiles A [`logical`] scalar: should sample quantiles be used as
#'  confidence interval? If `TRUE` (the default), sample quantiles are used as
#'  described in Kintigh (1989), else quantiles of the normal distribution are
#'  used.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param step A non-negative [`integer`] giving the increment of the
#'  sample size. Only used if `simulate` is `TRUE`.
#' @param progress A [`logical`] scalar: should a progress bar be displayed?
#' @inheritParams stats_bootstrap
#' @param ... Further arguments to be passed to internal methods.
#' @details
#'  *Diversity* measurement assumes that all individuals in a specific
#'  taxa are equivalent and that all types are equally different from each
#'  other (Peet 1974). A measure of diversity can be achieved by using indices
#'  built on the relative abundance of taxa. These indices (sometimes referred
#'  to as non-parametric indices) benefit from not making assumptions about the
#'  underlying distribution of taxa abundance: they only take relative
#'  abundances of the species that are present and species richness into
#'  account. Peet (1974) refers to them as indices of *heterogeneity*.
#'
#'  Diversity indices focus on one aspect of the taxa abundance and emphasize
#'  either *richness* (weighting towards uncommon taxa) or dominance (weighting
#'  towards abundant taxa; Magurran 1988).
#'
#'  *Evenness* is a measure of how evenly individuals are distributed across the
#'  sample.
#'
#'  The following heterogeneity index and corresponding evenness measures
#'  are available (see Magurran 1988 for details):
#'  \describe{
#'   \item{`berger`}{Berger-Parker dominance index. The Berger-Parker index
#'    expresses the proportional importance of the most abundant type. This
#'    metric is highly biased by sample size and richness, moreover it does not
#'    make use of all the information available from sample.}
#'   \item{`brillouin`}{Brillouin diversity index. The Brillouin index describes
#'    a known collection: it does not assume random sampling in an infinite
#'    population. Pielou (1975) and Laxton (1978) argues for the use of the
#'    Brillouin index in all circumstances, especially in preference to the
#'    Shannon index.}
#'   \item{`mcintosh`}{McIntosh dominance index. The McIntosh index expresses
#'    the heterogeneity of a sample in geometric terms. It describes the sample
#'    as a point of a \eqn{S}-dimensional hypervolume and uses the Euclidean
#'    distance of this point from the origin.}
#'   \item{`shannon`}{Shannon-Wiener diversity index. The Shannon index assumes
#'    that individuals are randomly sampled from an infinite population and that
#'    all taxa are represented in the sample (it does not reflect the
#'    sample size). The main source of error arises from the failure to include
#'    all taxa in the sample: this error increases as the proportion of species
#'    discovered in the sample declines (Peet 1974, Magurran 1988). The
#'    maximum likelihood estimator (MLE) is used for the relative abundance,
#'    this is known to be negatively biased by sample size.}
#'   \item{`simpson`}{Simpson dominance index for finite sample. The Simpson
#'    index expresses the probability that two individuals randomly picked from
#'    a finite sample belong to two different types. It can be interpreted as
#'    the weighted mean of the proportional abundances. This metric is a true
#'    probability value, it ranges from \eqn{0} (perfectly uneven) to \eqn{1}
#'    (perfectly even).}
#'  }
#'
#'  The `berger`, `mcintosh` and `simpson` methods return a *dominance* index,
#'  not the reciprocal or inverse form usually adopted, so that an increase in
#'  the value of the index accompanies a decrease in diversity.
#' @return
#'  * `index_heterogeneity()`, `index_evenness()` and `simulate_evenness()`
#'    return a [`DiversityIndex-class`] object.
#'  * `bootstrap_*()` and `jackknife_*()` return a [`data.frame`].
#' @note
#'  Ramanujan approximation is used for \eqn{x!} computation if \eqn{x > 170}.
#' @references
#'  Berger, W. H. & Parker, F. L. (1970). Diversity of Planktonic Foraminifera
#'  in Deep-Sea Sediments. *Science*, 168(3937), 1345-1347.
#'  \doi{10.1126/science.168.3937.1345}.
#'
#'  Brillouin, L. (1956). *Science and information theory*. New York:
#'  Academic Press.
#'
#'  Kintigh, K. W. (1984). Measuring Archaeological Diversity by Comparison
#'  with Simulated Assemblages. *American Antiquity*, 49(1), 44-54.
#'  \doi{10.2307/280511}.
#'
#'  Kintigh, K. W. (1989). Sample Size, Significance, and Measures of
#'  Diversity. In Leonard, R. D. and Jones, G. T., *Quantifying Diversity
#'  in Archaeology*. New Directions in Archaeology. Cambridge:
#'  Cambridge University Press, p. 25-36.
#'
#'  Laxton, R. R. (1978). The measure of diversity. *Journal of Theoretical
#'  Biology*, 70(1), 51-67.
#'  \doi{10.1016/0022-5193(78)90302-8}.
#'
#'  Magurran, A. E. (1988). *Ecological Diversity and its Measurement*.
#'  Princeton, NJ: Princeton University Press.
#'  \doi{10.1007/978-94-015-7358-0}.
#'
#'  McIntosh, R. P. (1967). An Index of Diversity and the Relation of Certain
#'  Concepts to Diversity. *Ecology*, 48(3), 392-404.
#'  \doi{10.2307/1932674}.
#'
#'  Peet, R. K. (1974). The Measurement of Species Diversity. *Annual Review of
#'  Ecology and Systematics*, 5(1), 285-307.
#'  \doi{10.1146/annurev.es.05.110174.001441}.
#'
#'  Pielou, E. C. (1975). *Ecological Diversity*. New York: Wiley.
#'  \doi{10.4319/lo.1977.22.1.0174b}
#'
#'  Shannon, C. E. (1948). A Mathematical Theory of Communication. *The
#'  Bell System Technical Journal*, 27, 379-423.
#'  \doi{10.1002/j.1538-7305.1948.tb01338.x}.
#'
#'  Simpson, E. H. (1949). Measurement of Diversity. *Nature*, 163(4148),
#'  688-688. \doi{10.1038/163688a0}.
#' @example inst/examples/ex-diversity.R
#' @author N. Frerebeau
#' @family diversity
#' @seealso [plot_diversity()], [similarity()], [turnover()]
#' @docType methods
#' @name heterogeneity-index
#' @rdname heterogeneity-index
NULL

#' @rdname heterogeneity-index
#' @aliases index_heterogeneity-method
setGeneric(
  name = "index_heterogeneity",
  def = function(object, ...) standardGeneric("index_heterogeneity"),
  valueClass = "HeterogeneityIndex"
)

#' @rdname heterogeneity-index
#' @aliases simulate_heterogeneity-method
setGeneric(
  name = "simulate_heterogeneity",
  def = function(object, ...) standardGeneric("simulate_heterogeneity"),
  valueClass = "HeterogeneityIndex"
)

#' @rdname heterogeneity-index
#' @aliases bootstrap_heterogeneity-method
setGeneric(
  name = "bootstrap_heterogeneity",
  def = function(object, ...) standardGeneric("bootstrap_heterogeneity"),
  valueClass = "data.frame"
)

#' @rdname heterogeneity-index
#' @aliases jackknife_heterogeneity-method
setGeneric(
  name = "jackknife_heterogeneity",
  def = function(object, ...) standardGeneric("jackknife_heterogeneity"),
  valueClass = "data.frame"
)

#' @rdname heterogeneity-index
#' @aliases index_evenness-method
setGeneric(
  name = "index_evenness",
  def = function(object, ...) standardGeneric("index_evenness"),
  valueClass = "EvennessIndex"
)

#' @rdname heterogeneity-index
#' @aliases simulate_evenness-method
setGeneric(
  name = "simulate_evenness",
  def = function(object, ...) standardGeneric("simulate_evenness"),
  valueClass = "EvennessIndex"
)

#' @rdname heterogeneity-index
#' @aliases bootstrap_evenness-method
setGeneric(
  name = "bootstrap_evenness",
  def = function(object, ...) standardGeneric("bootstrap_evenness"),
  valueClass = "data.frame"
)

#' @rdname heterogeneity-index
#' @aliases jackknife_evenness-method
setGeneric(
  name = "jackknife_evenness",
  def = function(object, ...) standardGeneric("jackknife_evenness"),
  valueClass = "data.frame"
)

## Richness --------------------------------------------------------------------
#' Richness and Rarefaction
#'
#' @description
#'  * `index_richness()` returns sample richness. `index_composition()` returns
#'    asymptotic species richness.
#'  * `rarefaction()` returns Hurlbert's unbiased estimate of Sander's
#'    rarefaction.
#'  * `bootstrap_*()` and `jackknife_*()` perform bootstrap/jackknife
#'    resampling.
#' @param object A \eqn{m \times p}{m x p} matrix of count data.
#' @param method A [`character`] string or vector of strings specifying the
#' index to be computed (see details). Any unambiguous substring can be given.
#' @param quantiles A [`logical`] scalar: should sample quantiles be used as
#'  confidence interval? If `TRUE` (the default), sample quantiles are used as
#'  described in Kintigh (1989), else quantiles of the normal distribution are
#'  used.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param step A non-negative [`integer`] giving the increment of the sample
#'  size. Only used if `simulate` is `TRUE`.
#' @param unbiased A [`logical`] scalar. Should the bias-corrected estimator be
#'  used? Only used with "`chao1`" or "`chao2`" (improved) estimator.
#' @param improved A [`logical`] scalar. Should the improved estimator be used?
#'  Only used with "`chao1`" or "`chao2`".
#' @param sample A length-one [`numeric`] vector giving the sub-sample size.
#' @param k A length-one [`numeric`] vector giving the threshold between
#'  rare/infrequent and abundant/frequent species. Only used if `method` is
#'  "`ace`" or "`ice`".
#' @param progress A [`logical`] scalar: should a progress bar be displayed?
#' @param simplify A [`logical`] scalar: should the result be simplified to a
#'  matrix? The default value, `FALSE`, returns a list.
#' @inheritParams stats_bootstrap
#' @param ... Further arguments to be passed to internal methods.
#' @details
#'  The number of different taxa, provides an instantly comprehensible
#'  expression of diversity. While the number of taxa within a sample
#'  is easy to ascertain, as a term, it makes little sense: some taxa
#'  may not have been seen, or there may not be a fixed number of taxa
#'  (e.g. in an open system; Peet 1974). As an alternative, *richness*
#'  (\eqn{S}) can be used for the concept of taxa number (McIntosh 1967).
#'
#'  It is not always possible to ensure that all sample sizes are equal
#'  and the number of different taxa increases with sample size and
#'  sampling effort (Magurran 1988). Then, *rarefaction* (\eqn{E(S)}) is
#'  the number of taxa expected if all samples were of a standard size (i.e.
#'  taxa per fixed number of individuals). Rarefaction assumes that imbalances
#'  between taxa are due to sampling and not to differences in actual
#'  abundances.
#'
#'  The following richness measures are available for count data:
#'  \describe{
#'   \item{`margalef`}{Margalef richness index.}
#'   \item{`menhinick`}{Menhinick richness index.}
#'   \item{`none`}{Returns the number of observed taxa/types.}
#'  }
#'
#' @section Asymptotic Species Richness:
#'  The following measures are available for count data:
#'  \describe{
#'   \item{`ace`}{Abundance-based Coverage Estimator.}
#'   \item{`chao1`}{(improved/unbiased) Chao1 estimator.}
#'  }
#'
#'  The following measures are available for replicated incidence data:
#'  \describe{
#'   \item{`ice`}{Incidence-based Coverage Estimator.}
#'   \item{`chao2`}{(improved/unbiased) Chao2 estimator.}
#'  }
#' @return
#'  * `index_richness()`, `simulate_richness()` and `index_composition()` return a
#'    [`DiversityIndex-class`] object.
#'  * `bootstrap_*()` and `jackknife_*()` return a [`data.frame`].
#'
#'  If `simplify` is `FALSE`, then `rarefaction()` returns a [`list`] (default),
#'  else return a [`matrix`].
#' @references
#'  Chao, A. (1984). Nonparametric Estimation of the Number of Classes in a
#'  Population. *Scandinavian Journal of Statistics*, 11(4), 265-270.
#'
#'  Chao, A. (1987). Estimating the Population Size for Capture-Recapture Data
#'  with Unequal Catchability. *Biometrics* 43(4), 783-791.
#'  \doi{10.2307/2531532}.
#'
#'  Chao, A. & Chiu, C.-H. (2016). Species Richness: Estimation and Comparison.
#'  *In* Balakrishnan, N., Colton, T., Everitt, B., Piegorsch, B., Ruggeri,
#'  F. & Teugels, J. L. (Eds.), *Wiley StatsRef: Statistics Reference Online*.
#'  Chichester, UK: John Wiley & Sons, Ltd., 1-26.
#'  \doi{10.1002/9781118445112.stat03432.pub2}
#'
#'  Chao, A. & Lee, S.-M. (1992). Estimating the Number of Classes via Sample
#'  Coverage. *Journal of the American Statistical Association*, 87(417),
#'  210-217. \doi{10.1080/01621459.1992.10475194}.
#'
#'  Chiu, C.-H., Wang, Y.-T., Walther, B. A. & Chao, A. (2014). An improved
#'  nonparametric lower bound of species richness via a modified good-turing
#'  frequency formula. *Biometrics*, 70(3), 671-682.
#'  \doi{10.1111/biom.12200}.
#'
#'  Hurlbert, S. H. (1971). The Nonconcept of Species Diversity: A Critique and
#'  Alternative Parameters. *Ecology*, 52(4), 577-586.
#'  \doi{10.2307/1934145}.
#'
#'  Magurran, A. E. (1988). *Ecological Diversity and its Measurement*.
#'  Princeton, NJ: Princeton University Press. \doi{10.1007/978-94-015-7358-0}.
#'
#'  Kintigh, K. W. (1989). Sample Size, Significance, and Measures of
#'  Diversity. In Leonard, R. D. and Jones, G. T., *Quantifying Diversity
#'  in Archaeology*. New Directions in Archaeology. Cambridge:
#'  Cambridge University Press, p. 25-36.
#'
#'  Magurran, A E. & Brian J. McGill (2011). *Biological Diversity:
#'  Frontiers in Measurement and Assessment*. Oxford: Oxford University Press.
#'
#'  Margalef, R. (1958). Information Theory in Ecology. *General Systems*,
#'  3, 36-71.
#'
#'  Menhinick, E. F. (1964). A Comparison of Some Species-Individuals Diversity
#'  Indices Applied to Samples of Field Insects. *Ecology*, 45(4), 859-861.
#'  \doi{10.2307/1934933}.
#'
#'  McIntosh, R. P. (1967). An Index of Diversity and the Relation of Certain
#'  Concepts to Diversity. *Ecology*, 48(3), 392-404.
#'  \doi{10.2307/1932674}.
#'
#'  Sander, H. L. (1968). Marine Benthic Diversity: A Comparative Study.
#'  *The American Naturalist*, 102(925), 243-282.
#' @seealso [plot_diversity()]
#' @example inst/examples/ex-richness.R
#' @author N. Frerebeau
#' @family diversity
#' @docType methods
#' @name richness-index
#' @rdname richness-index
NULL

#' @rdname richness-index
#' @aliases index_richness-method
setGeneric(
  name = "index_richness",
  def = function(object, ...) standardGeneric("index_richness"),
  valueClass = "RichnessIndex"
)

#' @rdname richness-index
#' @aliases simulate_richness-method
setGeneric(
  name = "simulate_richness",
  def = function(object, ...) standardGeneric("simulate_richness"),
  valueClass = "RichnessIndex"
)

#' @rdname richness-index
#' @aliases bootstrap_richness-method
setGeneric(
  name = "bootstrap_richness",
  def = function(object, ...) standardGeneric("bootstrap_richness"),
  valueClass = "data.frame"
)

#' @rdname richness-index
#' @aliases jackknife_richness-method
setGeneric(
  name = "jackknife_richness",
  def = function(object, ...) standardGeneric("jackknife_richness"),
  valueClass = "data.frame"
)

#' @rdname richness-index
#' @aliases index_composition-method
setGeneric(
  name = "index_composition",
  def = function(object, ...) standardGeneric("index_composition")
)

#' @rdname richness-index
#' @aliases rarefaction-method
setGeneric(
  name = "rarefaction",
  def = function(object, ...) standardGeneric("rarefaction")
)

# Plot =========================================================================
## Diversity Plot --------------------------------------------------------------
#' Diversity Plot
#'
#' @param x A [`DiversityIndex-class`] object to be plotted.
#' @example inst/examples/ex-plot_diversity.R
#' @author N. Frerebeau
#' @family plot
#' @seealso [index_heterogeneity()], [index_evenness()], [index_richness()]
#' @docType methods
#' @name plot_diversity
#' @rdname plot_diversity
NULL

## Bar Plot --------------------------------------------------------------------
#' Bar Plot
#'
#' Plots a Bertin, Ford (battleship curve) or Dice-Leraas diagram.
#' @param object An abundance matrix to be plotted.
#' @param threshold A [`function`] that takes a numeric vector as argument and
#'  returns a numeric threshold value (see below). If `NULL` (the default), no
#'  threshold is computed.
#' @param scale A [`function`] used to scale each variable, that takes a numeric
#'  vector as argument and returns a numeric vector. If `NULL` (the default), no
#'  scaling is performed.
#' @param EPPM A [`logical`] scalar: should the EPPM be drawn (see below)?
#' @param ... Currently not used.
#' @details
#'  If `EPPM` is `TRUE` and if a relative abundance is greater than
#'  the mean percentage of the type, the exceeding part is highlighted.
#' @section Bertin Matrix:
#'  As de Falguerolles *et al.* (1997) points out:
#'  "In abstract terms, a Bertin matrix is a matrix
#'  of  displays. [...] To fix ideas, think of a data matrix, variable by case,
#'  with real valued variables. For each variable, draw a bar chart of variable
#'  value by case. High-light all bars representing a value above some sample
#'  threshold for that variable."
# @section Ford Diagram:
#' @inheritSection independance EPPM
#' @return
#'  A [ggplot2::ggplot] object.
#' @references
#'  Bertin, J. (1977). *La graphique et le traitement graphique de
#'  l'information*. Paris: Flammarion. Nouvelle Bibliothèque Scientifique.
#'
#'  de Falguerolles, A., Friedrich, F. & Sawitzki, G. (1997). A Tribute to J.
#'  Bertin's Graphical Data Analysis. In W. Badilla & F. Faulbaum (eds.),
#'  *SoftStat '97: Advances in Statistical Software 6*. Stuttgart: Lucius
#'  & Lucius, p. 11-20.
#'
#'  Desachy, B. (2004). Le sériographe EPPM: un outil informatisé de sériation
#'  graphique pour tableaux de comptages. *Revue archéologique de Picardie*,
#'  3(1), 39-56. \doi{10.3406/pica.2004.2396}.
#'
#'  Ford, J. A. (1962). *A quantitative method for deriving cultural
#'  chronology*. Washington, DC: Pan American Union. Technical manual 1.
#' @example inst/examples/ex-plot_bar.R
#' @author N. Frerebeau
#' @seealso [eppm()]
#' @family plot
#' @docType methods
#' @name plot_bar
#' @rdname plot_bar
#' @aliases seriographe Bertin Ford
NULL

#' @rdname plot_bar
#' @aliases plot_bertin-method
setGeneric(
  name = "plot_bertin",
  def = function(object, ...) standardGeneric("plot_bertin")
)

#' @rdname plot_bar
#' @aliases plot_ford-method
setGeneric(
  name = "plot_ford",
  def = function(object, ...) standardGeneric("plot_ford")
)

## Heatmap ---------------------------------------------------------------------
#' Heatmap
#'
#' Plots a heatmap.
#' @param object An object to be plotted.
#' @param ... Further arguments to be passed to internal methods.
#' @return
#'  A [ggplot2::ggplot] object.
#' @references
#'  Desachy, B. (2004). Le sériographe EPPM: un outil informatisé de sériation
#'  graphique pour tableaux de comptages. *Revue archéologique de Picardie*,
#'  3(1), 39-56. \doi{10.3406/pica.2004.2396}.
#' @example inst/examples/ex-plot_matrix.R
#' @author N. Frerebeau
#' @seealso [pvi()]
#' @family plot
#' @docType methods
#' @name plot_matrix
#' @rdname plot_matrix
#' @aliases matrigraphe
NULL

#' @rdname plot_matrix
#' @aliases plot_heatmap-method
setGeneric(
  name = "plot_heatmap",
  def = function(object, ...) standardGeneric("plot_heatmap")
)

## Line Plot -------------------------------------------------------------------
#' Line Plot
#'
#' Plots a rank *vs* relative abundance diagram.
#' @param object An abundance matrix to be plotted.
#' @param log A [`character`] string which contains "`x`" if the x axis is to be
#'  logarithmic, "`y`" if the y axis is to be logarithmic and "`xy`" or "`yx`"
#'  if both axes are to be logarithmic (base 10).
#' @param facet A [`logical`] scalar: should a matrix of panels defined by
#'  case/sample be drawn?
#' @param ... Further arguments to be passed to internal methods.
#' @return
#'  A [ggplot2::ggplot] object.
#' @references
#'  Magurran, A. E. (1988). *Ecological Diversity and its Measurement*.
#'  Princeton, NJ: Princeton University Press. \doi{10.1007/978-94-015-7358-0}.
#' @example inst/examples/ex-plot_rank.R
#' @author N. Frerebeau
#' @family plot
#' @docType methods
#' @name plot_line
#' @rdname plot_line
NULL

#' @rdname plot_line
#' @aliases plot_rank-method
setGeneric(
  name = "plot_rank",
  def = function(object, ...) standardGeneric("plot_rank")
)

## Spot Plot -------------------------------------------------------------------
#' Spot Plot
#'
#' Plots a spot matrix.
#' @param object An abundance matrix to be plotted.
#' @param threshold A [`function`] that takes a numeric vector as argument and
#'  returns a numeric threshold value. If `NULL` (the default), no threshold is
#'  computed.
#' @param diag A [`logical`] scalar indicating whether the diagonal of the
#'  matrix should be plotted.
#' @param upper A [`logical`] scalar indicating whether the upper triangle of
#'  the matrix should be plotted.
#' @param ... Extra parameters to be passed to `threshold`.
#' @details
#'  The spot matrix can be considered as a variant of the
#'  [Bertin diagram][plot_bertin()] where the data are first transformed to
#'  relative frequencies.
#' @return
#'  A [ggplot2::ggplot] object.
#' @note
#'  Adapted from Dan Gopstein's original
#'  [idea](https://dgopstein.github.io/articles/spot-matrix/).
#' @example inst/examples/ex-plot_spot.R
#' @author N. Frerebeau
#' @family plot
#' @docType methods
#' @name plot_spot
#' @rdname plot_spot
NULL

#' @rdname plot_spot
#' @aliases plot_spot-method
setGeneric(
  name = "plot_spot",
  def = function(object, ...) standardGeneric("plot_spot")
)

# Seriate ======================================================================
#' Matrix Seriation
#'
#' @description
#'  * `seriate_*()` computes a permutation order for rows and/or columns.
#'  * `permute()` rearranges a data matrix according to a permutation order.
#'  * `get_order()` returns the seriation order for rows and columns.
#' @param object,x An \eqn{m \times p}{m x p} data matrix (typically an object
#'  of class [`CountMatrix-class`] or [`IncidenceMatrix-class`].
#' @param order A [`PermutationOrder-class`] object giving the permutation
#'  order for rows and columns.
#' @param EPPM A [`logical`] scalar: should the seriation be computed on EPPM
#'  instead of raw data?
#' @param margin A [`numeric`] vector giving the subscripts which the
#'  rearrangement will be applied over: `1` indicates rows, `2` indicates
#'  columns, `c(1, 2)` indicates rows then columns, `c(2, 1)` indicates columns
#'  then rows.
#' @param axes An [`integer`] vector giving the subscripts of the CA axes to be
#'  used.
#' @param stop An [`integer`] giving the stopping rule (i.e. maximum number of
#'  iterations) to avoid infinite loop.
#' @param cutoff A function that takes a numeric vector as argument and returns
#'  a single numeric value (see below).
#' @param n A non-negative [`integer`] giving the number of bootstrap
#'  replications.
#' @param progress A [`logical`] scalar: should a progress bar be displayed?
#' @param ... Further arguments to be passed to internal methods.
#' @section Seriation:
#'  The matrix seriation problem in archaeology is based on three conditions
#'  and two assumptions, which Dunell (1970) summarizes as follows.
#'
#'  The homogeneity conditions state that all the groups included in a
#'  seriation must:
#'  \enumerate{
#'   \item{Be of comparable duration.}
#'   \item{Belong to the same cultural tradition.}
#'   \item{Come from the same local area.}
#'  }
#'
#'  The mathematical assumptions state that the distribution of any historical
#'  or temporal class:
#'  \enumerate{
#'   \item{Is continuous through time.}
#'   \item{Exhibits the form of a unimodal curve.}
#'  }
#'  Theses assumptions create a distributional model and ordering is
#'  accomplished by arranging the matrix so that the class distributions
#'  approximate the required pattern. The resulting order is inferred
#'  to be chronological.
#'
#'  The following seriation methods are available:
#'  \describe{
#'   \item{`seriate_average()`}{Correspondence analysis-based seriation
#'   (average ranking). Correspondence analysis (CA) is an effective method for
#'   the seriation of archaeological assemblages. The order of the rows and
#'   columns is given by the coordinates along one dimension of the CA space,
#'   assumed to account for temporal variation. The direction of temporal change
#'   within the correspondence analysis space is arbitrary: additional
#'   information is needed to determine the actual order in time.}
#'   \item{`seriate_rank()`}{Reciprocal ranking seriation. These procedures
#'   iteratively rearrange rows and/or columns according to their weighted rank
#'   in the data matrix until convergence.
#'   Note that this procedure could enter into an infinite loop.
#'   If no convergence is reached before the maximum number of iterations, it
#'   stops with a warning.}
#'  }
#' @section Correspondence Analysis:
#'  `refine_seriation()` allows to identify samples that are subject to
#'  sampling error or samples that have underlying structural relationships
#'  and might be influencing the ordering along the CA space.
#'
#'  This relies on a partial bootstrap approach to CA-based seriation where each
#'  sample is replicated `n` times. The maximum dimension length of
#'  the convex hull around the sample point cloud allows to remove samples for
#'  a given `cutoff` value.
#'
#'  According to Peebles and Schachner (2012), "\[this\] point removal procedure
#'  \[results in\] a reduced dataset where the position of individuals within the
#'  CA are highly stable and which produces an ordering consistent with the
#'  assumptions of frequency seriation."
#' @return
#'  * `seriate_*()` returns a [`PermutationOrder-class`] object.
#'  * `refine_seriation()` returns a [`RefineCA-class`] object.
#'  * `permute()` returns either a permuted [`CountMatrix-class`] or an
#'    [`IncidenceMatrix-class`] (the same as `object`).
#' @note
#'  Refining method can lead to much longer execution times and larger output
#'  objects.
#' @references
#'  Desachy, B. (2004). Le sériographe EPPM: un outil informatisé de sériation
#'  graphique pour tableaux de comptages. *Revue archéologique de Picardie*,
#'  3(1), 39-56. \doi{10.3406/pica.2004.2396}.
#'
#'  Dunnell, R. C. (1970). Seriation Method and Its Evaluation. *American
#'  Antiquity*, 35(03), 305-319. \doi{10.2307/278341}.
#'
#'  Ihm, P. (2005). A Contribution to the History of Seriation in Archaeology.
#'  In C. Weihs & W. Gaul (Eds.), *Classification: The Ubiquitous
#'  Challenge*. Berlin Heidelberg: Springer, p. 307-316.
#'  \doi{10.1007/3-540-28084-7_34}.
#'
#'  Peeples, M. A., & Schachner, G. (2012). Refining correspondence
#'  analysis-based ceramic seriation of regional data sets. *Journal of
#'  Archaeological Science*, 39(8), 2818-2827.
#'  \doi{10.1016/j.jas.2012.04.040}.
#' @seealso [dimensio::ca()]
#' @example inst/examples/ex-seriation.R
#' @author N. Frerebeau
#' @family seriation
#' @docType methods
#' @name seriation
#' @rdname seriation
NULL

#' @rdname seriation
#' @aliases seriate_average-method
setGeneric(
  name = "seriate_average",
  def = function(object, ...) standardGeneric("seriate_average"),
  valueClass = "PermutationOrder"
)

#' @rdname seriation
#' @aliases seriate_rank-method
setGeneric(
  name = "seriate_rank",
  def = function(object, ...) standardGeneric("seriate_rank"),
  valueClass = "PermutationOrder"
)

# @rdname seriation
# @aliases seriate_idds-method
# setGeneric(
#   name = "seriate_idds",
#   def = function(object, ...) standardGeneric("seriate_idds")
# )

#' @rdname seriation
#' @aliases permute-method
setGeneric(
  name = "permute",
  def = function(object, order, ...) standardGeneric("permute")
)

#' @rdname seriation
#' @aliases refine_seriation-method
setGeneric(
  name = "refine_seriation",
  def = function(object, ...) standardGeneric("refine_seriation"),
  valueClass = "RefineCA"
)

# Similarity ===================================================================
#' Similarity
#'
#' @param object A \eqn{m \times p}{m x p} matrix of count data.
#' @param method A [`character`] string specifying the method to be
#'  used (see details). Any unambiguous substring can be given.
#' @param ... Further arguments to be passed to internal methods.
#' @details
#'  \eqn{\beta}-diversity can be measured by addressing *similarity*
#'  between pairs of samples/cases (Brainerd-Robinson, Jaccard, Morisita-Horn
#'  and Sorenson indices). Similarity between pairs of taxa/types can be
#'  measured by assessing the degree of co-occurrence (binomial co-occurrence).
#'
#'  Jaccard, Morisita-Horn and Sorenson indices provide a scale of similarity
#'  from \eqn{0}-\eqn{1} where \eqn{1} is perfect similarity and \eqn{0} is
#'  no similarity. The Brainerd-Robinson index is scaled between \eqn{0} and
#'  \eqn{200}. The Binomial co-occurrence assessment approximates a Z-score.
#'
#'  \describe{
#'   \item{`binomial`}{Binomial co-occurrence assessment. This assesses the
#'   degree of co-occurrence between taxa/types within a dataset. The strongest
#'   associations are shown by large positive numbers, the strongest
#'   segregations by large negative numbers.}
#'   \item{`brainerd`}{Brainerd-Robinson quantitative index. This is a
#'   city-block metric of similarity between pairs of samples/cases.}
#'   \item{`bray`}{Sorenson quantitative index (Bray and Curtis modified version
#'   of the Sorenson index).}
#'   \item{`jaccard`}{Jaccard qualitative index.}
#'   \item{`morisita`}{Morisita-Horn quantitative index.}
#'   \item{`sorenson`}{Sorenson qualitative index.}
#'  }
#' @return
#'  `similarity()` returns a [stats::dist] object.
#' @references
#'  Brainerd, G. W. (1951). The Place of Chronological Ordering in
#'  Archaeological Analysis. *American Antiquity*, 16(04), 301-313.
#'  \doi{10.2307/276979}.
#'
#'  Bray, J. R. & Curtis, J. T. (1957). An Ordination of the Upland Forest
#'  Communities of Southern Wisconsin. *Ecological Monographs*, 27(4),
#'  325-349. \doi{10.2307/1942268}.
#'
#'  Kintigh, K. (2006). Ceramic Dating and Type Associations. In J. Hantman and
#'  R. Most (eds.), *Managing Archaeological Data: Essays in Honor of
#'  Sylvia W. Gaines*. Anthropological Research Paper, 57. Tempe, AZ: Arizona
#'  State University, p. 17-26.
#'
#'  Magurran, A. E. (1988). *Ecological Diversity and its Measurement*.
#'  Princeton, NJ: Princeton University Press. \doi{10.1007/978-94-015-7358-0}.
#'
#'  Robinson, W. S. (1951). A Method for Chronologically Ordering Archaeological
#'  Deposits. *American Antiquity*, 16(04), 293-301. \doi{10.2307/276978}.
#' @example inst/examples/ex-similarity.R
#' @author N. Frerebeau
#' @family diversity
#' @docType methods
#' @rdname similarity-method
#' @aliases similarity-method
setGeneric(
  name = "similarity",
  def = function(object, ...) standardGeneric("similarity")
)

# Test =========================================================================
## Diversity Test --------------------------------------------------------------
#' Diversity Test
#'
#' Compares Shannon diversity between samples.
#' @param object A \eqn{m \times p}{m x p} matrix of count data.
#' @param adjust A [`character`] string specifying the method for
#'  adjusting \eqn{p} values (see [stats::p.adjust()]).
#' @param ... Further arguments to be passed to internal methods.
#' @details
#'  This test produces two sided pairwise comparisons: it returns a matrix of
#'  adjusted \eqn{p} values.
#' @return
#'  A [`numeric`] [`matrix`].
#' @example inst/examples/ex-test.R
#' @author N. Frerebeau
#' @references
#'  Magurran, A. E. (1988). *Ecological Diversity and its Measurement*.
#'  Princeton, NJ: Princeton University Press. \doi{10.1007/978-94-015-7358-0}.
#' @family statistics
#' @docType methods
#' @rdname test_diversity
#' @aliases test_diversity-method
setGeneric(
  name = "test_diversity",
  def = function(object, ...) standardGeneric("test_diversity")
)

# Turnover =====================================================================
#' Turnover
#'
#' Returns the degree of turnover in taxa composition along a gradient or
#' transect.
#' @param object A \eqn{m \times p}{m x p} matrix of count data.
#' @param method A [`character`] string specifying the method to be
#'  used (see details). Any unambiguous substring can be given.
#' @param simplify A [`logical`] scalar: should the result be
#'  simplified to a matrix?
#' @param ... Further arguments to be passed to internal methods.
#' @details
#'  The following methods can be used to ascertain the degree of *turnover*
#'  in taxa composition along a gradient (\eqn{\beta}-diversity) on qualitative
#'  (presence/absence) data. This assumes that the order of the matrix rows
#'  (from \eqn{1} to \eqn{n}) follows the progression along the
#'  gradient/transect.
#'
#'  \describe{
#'   \item{`whittaker`}{Whittaker measure.}
#'   \item{`cody`}{Cody measure.}
#'   \item{`routledge1`}{Routledge first measure.}
#'   \item{`routledge2`}{Routledge second measure.}
#'   \item{`routledge3`}{Routledge third measure. This is the exponential form
#'   of the second measure.}
#'   \item{`wilson`}{Wilson measure.}
#'  }
#' @return
#'  If `simplify` is `FALSE`, returns a [`list`] (default), else returns a
#'  [`matrix`].
#' @references
#'  Cody, M. L. (1975). Towards a theory of continental species diversity: Bird
#'  distributions over Mediterranean habitat gradients. *In* M. L. Cody &
#'  J. M. Diamond (Eds.), *Ecology and Evolution of Communities*.
#'  Cambridge, MA: Harvard University Press, p. 214-257.
#'
#'  Routledge, R. D. (1977). On Whittaker's Components of Diversity.
#'  *Ecology*, 58(5), 1120-1127. \doi{10.2307/1936932}.
#'
#'  Whittaker, R. H. (1960). Vegetation of the Siskiyou Mountains, Oregon and
#'  California. *Ecological Monographs*, 30(3), 279-338.
#'  \doi{10.2307/1943563}.
#'
#'  Wilson, M. V., & Shmida, A. (1984). Measuring Beta Diversity with
#'  Presence-Absence Data. *The Journal of Ecology*, 72(3), 1055-1064.
#'  \doi{10.2307/2259551}.
#' @example inst/examples/ex-turnover.R
#' @author N. Frerebeau
#' @family diversity
#' @docType methods
#' @name turnover-index
#' @rdname turnover-index
NULL

#' @rdname turnover-index
#' @aliases turnover-method
setGeneric(
  name = "turnover",
  def = function(object, ...) standardGeneric("turnover")
)

# Deprecated ===================================================================
# Deprecated Methods
#
# @param object An object.
# @param ... Currently not used.
# @author N. Frerebeau
# @docType methods
# @name deprecate
# @rdname deprecate
NULL
