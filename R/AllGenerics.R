# GENERIC METHODS
#' @include AllClasses.R
NULL

# S4 dispatch to base S3 generic ===============================================
setGeneric("autoplot", package = "ggplot2")
setGeneric("jackknife", package = "arkhe")
setGeneric("bootstrap", package = "arkhe")

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
#' @family mutators
#' @name mutators
#' @rdname mutators
#' @aliases get set
NULL

#' @rdname mutators
#' @aliases get_method-method
setGeneric(
  name = "get_method",
  def = function(x) standardGeneric("get_method")
)

# Statistic ====================================================================
#' Resampling Methods
#'
#' @description
#'  * `resample()` simulate observations from a multinomial distribution.
#'  * `bootstrap()` generate bootstrap estimations of a statistic.
#'  * `jackknife()` generate jackknife estimations of a statistic.
#' @param object A [`numeric`] vector of count data (absolute frequencies).
#' @param do A [`function`] that takes `object` as an argument
#'  and returns a single numeric value.
#' @param n A non-negative [`integer`] specifying the number of bootstrap
#'  replications.
#' @param size A non-negative [`integer`] specifying the sample size.
#' @param f A [`function`] that takes a single numeric vector (the result of
#'  `do`) as argument.
#' @param ... Extra arguments passed to `do`.
#' @return
#'  If `f` is `NULL`, `resample()` returns the `n` values of `do`. Else,
#'  returns the result of `f` applied to the `n` values of `do`.
#'
#'  If `f` is `NULL`, `bootstrap()` and `jackknife()` return a [`data.frame`]
#'  with the following elements (else, returns the result of `f` applied to the
#'  `n` values of `do`) :
#'  \describe{
#'   \item{original}{The observed value of `do` applied to `object`.}
#'   \item{mean}{The bootstrap/jackknife estimate of mean of `do`.}
#'   \item{bias}{The bootstrap/jackknife estimate of bias of `do`.}
#'   \item{error}{The boostrap/jackknife estimate of standard error of `do`.}
#'  }
#' @seealso [stats::rmultinom()]
#' @example inst/examples/ex-resample.R
#' @author N. Frerebeau
#' @docType methods
#' @family resampling methods
#' @aliases resample-method
setGeneric(
  name = "resample",
  def = function(object, ...) standardGeneric("resample")
)

# Diversity ====================================================================
## Heterogeneity ---------------------------------------------------------------
#' Heterogeneity and Evenness
#'
#' @description
#'  * `heterogeneity()` returns an heterogeneity or dominance index.
#'  * `evenness()` returns an evenness measure.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each class).
#' @param method A [`character`] string specifying the index to be computed
#'  (see details). Any unambiguous substring can be given.
#' @param x A [`numeric`] vector of count data (absolute frequencies).
#' @param evenness A [`logical`] scalar: should an evenness measure be computed
#'  instead of an heterogeneity/dominance index?
#' @param j An [`integer`] giving the index of the reference type/taxa.
#'  If `NULL` (the default), the most frequent type/taxa in any assemblage will
#'  be used.
#' @param base A positive [`numeric`] value specifying the base with respect to
#'  which logarithms are computed.
#' @param na.rm A [`numeric`] scalar: should missing values (including `NaN`) be
#'  removed?
#' @param ... Currently not used.
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
#' @section Heterogeneity and Evenness Measures:
#'  The following heterogeneity index and corresponding evenness measures
#'  are available (see Magurran 1988 for details):
#'  \describe{
#'   \item{`berger`}{Berger-Parker dominance index. The Berger-Parker index
#'    expresses the proportional importance of the most abundant type. This
#'    metric is highly biased by sample size and richness, moreover it does not
#'    make use of all the information available from sample.}
#'   \item{`boone`}{Boone heterogeneity measure.}
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
#'  * `heterogeneity()` returns an [HeterogeneityIndex-class] object.
#'  * `evenness()` returns an [EvennessIndex-class] object.
#'  * `index_*()` return a [`numeric`] vector.
#' @note
#'  Ramanujan approximation is used for \eqn{x!} computation if \eqn{x > 170}.
#' @references
#'  Berger, W. H. & Parker, F. L. (1970). Diversity of Planktonic Foraminifera
#'  in Deep-Sea Sediments. *Science*, 168(3937), 1345-1347.
#'  \doi{10.1126/science.168.3937.1345}.
#'
#'  Boone, J. L. (1987). Defining and Measuring Midden Catchment. *American
#'  Antiquity*, 52(2), 336-45. \doi{10.2307/281785}.
#'
#'  Brillouin, L. (1956). *Science and information theory*. New York:
#'  Academic Press.
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
#' @family diversity measures
#' @docType methods
#' @aliases heterogeneity-method
setGeneric(
  name = "heterogeneity",
  def = function(object, ...) standardGeneric("heterogeneity"),
  valueClass = "HeterogeneityIndex"
)

#' @rdname heterogeneity
#' @aliases evenness-method
setGeneric(
  name = "evenness",
  def = function(object, ...) standardGeneric("evenness"),
  valueClass = "EvennessIndex"
)

#' @rdname heterogeneity
#' @aliases index_berger-method
setGeneric(
  name = "index_berger",
  def = function(x, ...) standardGeneric("index_berger")
)

#' @rdname heterogeneity
#' @aliases index_boone-method
setGeneric(
  name = "index_boone",
  def = function(x, ...) standardGeneric("index_boone")
)

#' @rdname heterogeneity
#' @aliases index_brillouin-method
setGeneric(
  name = "index_brillouin",
  def = function(x, ...) standardGeneric("index_brillouin")
)

#' @rdname heterogeneity
#' @aliases index_mcintosh-method
setGeneric(
  name = "index_mcintosh",
  def = function(x, ...) standardGeneric("index_mcintosh")
)

#' @rdname heterogeneity
#' @aliases index_shannon-method
setGeneric(
  name = "index_shannon",
  def = function(x, ...) standardGeneric("index_shannon")
)

#' @rdname heterogeneity
#' @aliases index_simpson-method
setGeneric(
  name = "index_simpson",
  def = function(x, ...) standardGeneric("index_simpson")
)

## Richness --------------------------------------------------------------------
#' Richness
#'
#' @description
#'  * `richness()` returns sample richness.
#'  * `composition()` returns asymptotic species richness.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies).
#' @param method A [`character`] string or vector of strings specifying the
#' index to be computed (see details). Any unambiguous substring can be given.
#' @param x A [`numeric`] vector or matrix of count data (absolute frequencies).
#' @param unbiased A [`logical`] scalar. Should the bias-corrected estimator be
#'  used? Only used with "`chao1`" or "`chao2`" (improved) estimator.
#' @param improved A [`logical`] scalar. Should the improved estimator be used?
#'  Only used with "`chao1`" or "`chao2`".
#' @param k A length-one [`numeric`] vector giving the threshold between
#'  rare/infrequent and abundant/frequent species. Only used if `method` is
#'  "`ace`" or "`ice`".
#' @param na.rm A [`numeric`] scalar: should missing values (including `NaN`) be
#'  removed?
#' @param ... Further arguments to be passed to internal methods.
#' @section Details:
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
#' @section Richness Measures:
#'  The following richness measures are available for count data:
#'  \describe{
#'   \item{`count`}{Returns the number of observed taxa/types.}
#'   \item{`margalef`}{Margalef richness index.}
#'   \item{`menhinick`}{Menhinick richness index.}
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
#'  * `richness()` returns a [RichnessIndex-class] object.
#'  * `composition()` returns a [CompositionIndex-class] object.
#'  * `index_*()` return a [`numeric`] vector.
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
#' @seealso [plot_diversity()]
#' @example inst/examples/ex-richness.R
#' @author N. Frerebeau
#' @family diversity measures
#' @docType methods
#' @aliases richness-method
setGeneric(
  name = "richness",
  def = function(object, ...) standardGeneric("richness"),
  valueClass = "RichnessIndex"
)

#' @rdname richness
#' @aliases composition-method
setGeneric(
  name = "composition",
  def = function(object, ...) standardGeneric("composition"),
  valueClass = "CompositionIndex"
)

#' @rdname richness
#' @aliases index_ace-method
setGeneric(
  name = "index_ace",
  def = function(x, ...) standardGeneric("index_ace")
)

#' @rdname richness
#' @aliases index_ice-method
setGeneric(
  name = "index_ice",
  def = function(x, ...) standardGeneric("index_ice")
)

#' @rdname richness
#' @aliases index_chao1-method
setGeneric(
  name = "index_chao1",
  def = function(x, ...) standardGeneric("index_chao1")
)

#' @rdname richness
#' @aliases index_chao2-method
setGeneric(
  name = "index_chao2",
  def = function(x, ...) standardGeneric("index_chao2")
)

#' @rdname richness
#' @aliases index_margalef-method
setGeneric(
  name = "index_margalef",
  def = function(x, ...) standardGeneric("index_margalef")
)

#' @rdname richness
#' @aliases index_menhinick-method
setGeneric(
  name = "index_menhinick",
  def = function(x, ...) standardGeneric("index_menhinick")
)

## Rarefaction -----------------------------------------------------------------
#' Rarefaction
#'
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies).
#' @param x A [`numeric`] vector of count data (absolute frequencies).
#' @param sample A length-one [`numeric`] vector giving the sub-sample size.
#'  The size of sample should be smaller than total community size.
#' @param method A [`character`] string or vector of strings specifying the
#' index to be computed (see details). Any unambiguous substring can be given.
#' @param step An [`integer`] giving the increment of the sample size.
#' @param ... Currently not used.
#' @inheritSection richness Details
#' @section Rarefaction Measures:
#'  The following rarefaction measures are available for count data:
#'  \describe{
#'   \item{`baxter`}{Baxter's rarefaction.}
#'   \item{`hurlbert`}{Hurlbert's unbiased estimate of Sander's rarefaction.}
#'  }
#' @return
#'  * `rarefaction()` returns a [RarefactionIndex-class] object.
#'  * `index_*()` return a [`numeric`] vector.
#' @references
#'  Baxter, M. J. (2001). Methodological Issues in the Study of Assemblage
#'  Diversity. *American Antiquity*, 66(4), 715-725. \doi{10.2307/2694184}.
#'
#'  Hurlbert, S. H. (1971). The Nonconcept of Species Diversity: A Critique and
#'  Alternative Parameters. *Ecology*, 52(4), 577-586.
#'  \doi{10.2307/1934145}.
#'
#'  Sander, H. L. (1968). Marine Benthic Diversity: A Comparative Study.
#'  *The American Naturalist*, 102(925), 243-282.
#' @example inst/examples/ex-rarefaction.R
#' @author N. Frerebeau
#' @family diversity measures
#' @docType methods
#' @aliases rarefaction-method
setGeneric(
  name = "rarefaction",
  def = function(object, ...) standardGeneric("rarefaction")
)

#' @rdname rarefaction
#' @aliases index_baxter-method
setGeneric(
  name = "index_baxter",
  def = function(x, ...) standardGeneric("index_baxter")
)

#' @rdname rarefaction
#' @aliases index_hurlbert-method
setGeneric(
  name = "index_hurlbert",
  def = function(x, ...) standardGeneric("index_hurlbert")
)

## Similarity ------------------------------------------------------------------
#' Similarity
#'
#' @param object A \eqn{m \times p}{m x p} matrix of count data.
#' @param x,y A length-\eqn{p} [`numeric`] vector of count data.
#' @param method A [`character`] string specifying the method to be
#'  used (see details). Any unambiguous substring can be given.
#' @param ... Currently not used.
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
#'  * `similarity()` returns a [stats::dist] object.
#'  * `index_*()` return a [`numeric`] vector.
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
#' @family diversity measures
#' @docType methods
#' @aliases similarity-method
setGeneric(
  name = "similarity",
  def = function(object, ...) standardGeneric("similarity")
)

#' @rdname similarity
#' @aliases index_jaccard-method
setGeneric(
  name = "index_jaccard",
  def = function(x, y, ...) standardGeneric("index_jaccard")
)

#' @rdname similarity
#' @aliases index_sorenson-method
setGeneric(
  name = "index_sorenson",
  def = function(x, y, ...) standardGeneric("index_sorenson")
)

#' @rdname similarity
#' @aliases index_bray-method
setGeneric(
  name = "index_bray",
  def = function(x, y, ...) standardGeneric("index_bray")
)

#' @rdname similarity
#' @aliases index_morisita-method
setGeneric(
  name = "index_morisita",
  def = function(x, y, ...) standardGeneric("index_morisita")
)

#' @rdname similarity
#' @aliases index_brainerd-method
setGeneric(
  name = "index_brainerd",
  def = function(x, y, ...) standardGeneric("index_brainerd")
)

#' @rdname similarity
#' @aliases index_binomial-method
setGeneric(
  name = "index_binomial",
  def = function(x, y, ...) standardGeneric("index_binomial")
)

## Co-Occurrence ---------------------------------------------------------------
#' Co-Occurrence
#'
#' @param object A \eqn{m \times p}{m x p} matrix of count data.
#' @param ... Currently not used.
#' @details
#'  A co-occurrence matrix is a symmetric matrix with zeros on its main
#'  diagonal, which works out how many times each pairs of taxa/types occur
#'  together in at least one sample.
#' @return
#'  A [stats::dist] object.
#' @example inst/examples/ex-occurrence.R
#' @author N. Frerebeau
#' @family diversity measures
#' @docType methods
#' @aliases occurrence-method
setGeneric(
  name = "occurrence",
  def = function(object, ...) standardGeneric("occurrence")
)

## Turnover --------------------------------------------------------------------
#' Turnover
#'
#' Returns the degree of turnover in taxa composition along a gradient or
#' transect.
#' @param object,x A \eqn{m \times p}{m x p} matrix of count data or incidence
#'  data.
#' @param method A [`character`] string specifying the method to be
#'  used (see details). Any unambiguous substring can be given.
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
#'  A [`numeric`] vector.
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
#' @family diversity measures
#' @docType methods
#' @aliases turnover-method
setGeneric(
  name = "turnover",

  def = function(object, ...) standardGeneric("turnover")
)

#' @rdname turnover
#' @aliases index_whittaker-method
setGeneric(
  name = "index_whittaker",
  def = function(x, ...) standardGeneric("index_whittaker")
)

#' @rdname turnover
#' @aliases index_cody-method
setGeneric(
  name = "index_cody",
  def = function(x, ...) standardGeneric("index_cody")
)

#' @rdname turnover
#' @aliases index_routledge1-method
setGeneric(
  name = "index_routledge1",
  def = function(x, ...) standardGeneric("index_routledge1")
)

#' @rdname turnover
#' @aliases index_routledge2-method
setGeneric(
  name = "index_routledge2",
  def = function(x, ...) standardGeneric("index_routledge2")
)

#' @rdname turnover
#' @aliases index_routledge3-method
setGeneric(
  name = "index_routledge3",
  def = function(x, ...) standardGeneric("index_routledge3")
)

#' @rdname turnover
#' @aliases index_wilson-method
setGeneric(
  name = "index_wilson",
  def = function(x, ...) standardGeneric("index_wilson")
)

## Simulate --------------------------------------------------------------------
#' Measure Diversity by Comparing to Simulated Assemblages
#'
#' @param object A [DiversityIndex-class] object.
#' @param interval A [`character`] string giving the type of confidence
#'  interval to be returned. It must be one "`percentiles`" (sample quantiles,
#'  as described in Kintigh 1984; the default), "`student`" or "`normal`".
#'  Any unambiguous substring can be given.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param step An [`integer`] giving the increment of the sample size.
#' @param n A non-negative [`integer`] giving the number of bootstrap
#'  replications.
#' @param progress A [`logical`] scalar: should a progress bar be displayed?
#' @return
#'  Returns a [DiversityIndex-class] object.
#' @references
#'  Baxter, M. J. (2001). Methodological Issues in the Study of Assemblage
#'  Diversity. *American Antiquity*, 66(4), 715-725. \doi{10.2307/2694184}.
#'
#'  Kintigh, K. W. (1984). Measuring Archaeological Diversity by Comparison
#'  with Simulated Assemblages. *American Antiquity*, 49(1), 44-54.
#'  \doi{10.2307/280511}.
#' @seealso [plot_diversity()], [resample()]
#' @example inst/examples/ex-plot_diversity.R
#' @author N. Frerebeau
#' @family diversity measures
#' @docType methods
#' @name simulate
#' @rdname simulate
NULL

## Plot ------------------------------------------------------------------------
#' Diversity Plot
#'
#' @param object,x A [DiversityIndex-class] object to be plotted.
#' @param y Currently not used.
#' @param ... Currently not used.
#' @return
#'  * `autoplot()` returns a [`ggplot`][ggplot2::ggplot] object.
#'  * `plot()` is called it for its side-effects: it results in a graphic being
#'    displayed (invisibly returns `x`).
#' @example inst/examples/ex-plot_diversity.R
#' @author N. Frerebeau
#' @family diversity measures
#' @family plot methods
#' @docType methods
#' @name plot_diversity
#' @rdname plot_diversity
NULL

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
#' @aliases test_diversity-method
setGeneric(
  name = "test_diversity",
  def = function(object, ...) standardGeneric("test_diversity")
)

# Plot =========================================================================
## Matrix plot -----------------------------------------------------------------
### Heatmap --------------------------------------------------------------------
#' Heatmap
#'
#' Plots a heatmap.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each class).
#' @param diag A [`logical`] scalar indicating whether the diagonal of the
#'  matrix should be plotted. Only used if `object` is a symmetric matrix.
#' @param upper A [`logical`] scalar indicating whether the upper triangle of
#'  the matrix should be plotted. Only used if `object` is a symmetric matrix.
#' @param lower A [`logical`] scalar indicating whether the lower triangle of
#'  the matrix should be plotted. Only used if `object` is a symmetric matrix.
#' @param freq A [`logical`] scalar indicating whether relative frequency
#'  should be used instead of counts (absolute frequency).
#' @param ... Currently not used.
#' @return
#'  A [ggplot2::ggplot] object.
#' @example inst/examples/ex-plot_matrix.R
#' @author N. Frerebeau
#' @family plot methods
#' @docType methods
#' @aliases plot_heatmap-method
setGeneric(
  name = "plot_heatmap",
  def = function(object, ...) standardGeneric("plot_heatmap")
)

### Matrigraph -----------------------------------------------------------------
#' Matrigraph
#'
#' @description
#'  * `matrigraph()` produces a heatmap highlighting the deviations from
#'    independence.
#'  * `pvi()` computes for each cell of a numeric matrix the percentage to the
#'    column theoretical independence value.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each class).
#' @param reverse A [`logical`] scalar: should negative deviations be centered
#'  (see details)?
#' @param ... Currently not used.
#' @details
#'  PVI (in french "pourcentages de valeur d'indépendance") is calculated for
#'  each cell as the percentage to the column theoretical independence value:
#'  PVI greater than \eqn{1} represent positive deviations from the
#'  independence, whereas PVI smaller than \eqn{1} represent negative
#'  deviations (Desachy 2004).
#'
#'  The PVI matrix allows to explore deviations from independence (an
#'  intuitive approach to \eqn{\chi^2}{Chi-squared}), in such a way that a
#'  high-contrast matrix has quite significant deviations,
#'  with a low risk of being due to randomness (Desachy 2004).
#'
#'  `matrigraph()` displays the deviations from independence:
#'
#'  * If the PVI is equal to \eqn{1} (statistical independence), the cell of the
#'    matrix is filled in grey.
#'  * If the PVI is less than \eqn{1} (negative deviation from independence),
#'    the size of the grey square is proportional to the PVI (the white margin
#'    thus represents the fraction of negative deviation).
#'  * If the PVI is greater than \eqn{1} (positive deviation), a black
#'    square representing the fraction of positive deviations is
#'    superimposed. For large positive deviations (PVI greater than \eqn{2}),
#'    the cell in filled in black.
#'
#'  If `reverse` is `TRUE`, the fraction of negative deviations is displayed
#'  as a white square.
#' @references
#'  Desachy, B. (2004). Le sériographe EPPM: un outil informatisé de sériation
#'  graphique pour tableaux de comptages. *Revue archéologique de Picardie*,
#'  3(1), 39-56. \doi{10.3406/pica.2004.2396}.
#' @return
#'  * `matrigraph()` returns a [ggplot2::ggplot] object.
#'  * `pvi()` returns a [`numeric`] [`matrix`].
#' @example inst/examples/ex-matrigraph.R
#' @author N. Frerebeau
#' @seealso [plot_heatmap()]
#' @family plot methods
#' @docType methods
#' @aliases matrigraph-method
setGeneric(
  name = "matrigraph",
  def = function(object, ...) standardGeneric("matrigraph")
)

#' @rdname matrigraph
#' @aliases pvi-method
setGeneric(
  name = "pvi",
  def = function(object, ...) standardGeneric("pvi")
)

## Bar Plot --------------------------------------------------------------------
### Bertin ---------------------------------------------------------------------
#' Bertin Diagram
#'
#' Plots a Bertin diagram.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each class).
#' @param threshold A [`function`] that takes a numeric vector as argument and
#'  returns a numeric threshold value (see below). If `NULL` (the default), no
#'  threshold is computed.
#' @param scale A [`function`] used to scale each variable, that takes a numeric
#'  vector as argument and returns a numeric vector. If `NULL` (the default), no
#'  scaling is performed.
#' @param ... Currently not used.
#' @section Bertin Matrix:
#'  As de Falguerolles *et al.* (1997) points out:
#'  "In abstract terms, a Bertin matrix is a matrix
#'  of  displays. [...] To fix ideas, think of a data matrix, variable by case,
#'  with real valued variables. For each variable, draw a bar chart of variable
#'  value by case. High-light all bars representing a value above some sample
#'  threshold for that variable."
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
#' @example inst/examples/ex-plot_bertin.R
#' @author N. Frerebeau
#' @family plot methods
#' @docType methods
#' @aliases plot_bertin-method Bertin
setGeneric(
  name = "plot_bertin",
  def = function(object, ...) standardGeneric("plot_bertin")
)

### Ford -----------------------------------------------------------------------
#' Ford Diagram
#'
#' Plots a Ford (battleship curve) diagram.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each class).
#' @param EPPM A [`logical`] scalar: should the EPPM be drawn?
#'  This argument is defunct: use `seriograph()` instead.
#' @param ... Currently not used.
#' @return
#'  A [ggplot2::ggplot] object.
#' @references
#'  Ford, J. A. (1962). *A quantitative method for deriving cultural
#'  chronology*. Washington, DC: Pan American Union. Technical manual 1.
#' @example inst/examples/ex-plot_ford.R
#' @author N. Frerebeau
#' @family plot methods
#' @docType methods
#' @aliases plot_ford-method Ford
setGeneric(
  name = "plot_ford",
  def = function(object, ...) standardGeneric("plot_ford")
)

### Seriograph -----------------------------------------------------------------
#' Seriograph
#'
#' @description
#'  * `seriograph()` produces a Ford diagram highlighting the relationships
#'    between rows and columns.
#'  * `eppm()` computes for each cell of a numeric matrix the positive
#'    difference from the column mean percentage.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each class).
#' @param weights A [`logical`] scalar: should row weights (i.e. the number of
#'  observations divided by the total number of observations) be displayed?
#' @param ... Currently not used.
#' @details
#'  The positive difference from the column mean percentage (in french "écart
#'  positif au pourcentage moyen", EPPM) represents a deviation from the
#'  situation of statistical independence. As independence can be interpreted as
#'  the absence of relationships between types and the chronological order of
#'  the assemblages, EPPM is a useful tool to explore significance
#'  of relationship between rows and columns related to seriation (Desachy
#'  2004).
#'
#'  `seriograph()` superimposes the frequencies (grey) and EPPM values (black)
#'  for each row-column pair in a Ford diagram.
#' @references
#'  Desachy, B. (2004). Le sériographe EPPM: un outil informatisé de sériation
#'  graphique pour tableaux de comptages. *Revue archéologique de Picardie*,
#'  3(1), 39-56. \doi{10.3406/pica.2004.2396}.
#' @return
#'  * `seriograph()` returns a [ggplot2::ggplot] object.
#'  * `eppm()` returns a [`numeric`] [`matrix`].
#' @example inst/examples/ex-seriograph.R
#' @author N. Frerebeau
#' @seealso [plot_ford()]
#' @family plot methods
#' @docType methods
#' @aliases seriograph-method
setGeneric(
  name = "seriograph",
  def = function(object, ...) standardGeneric("seriograph")
)

#' @rdname seriograph
#' @aliases eppm-method
setGeneric(
  name = "eppm",
  def = function(object, ...) standardGeneric("eppm")
)

### Dice-Leraas ----------------------------------------------------------------
#' Dice-Leraas Diagram
#'
#' Plots a Dice-Leraas diagram.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each class).
#' @param ... Currently not used.
#' @details
#'  In a Dice-Leraas diagram, the horizontal line represents the range of data
#'  (min-max) and the small vertical line indicates the mean. The black
#'  rectangle is twice the standard error on the mean, while the white rectangle
#'  is one standard deviation on either side of the mean.
#' @references
#'  Dice, L. R., & Leraas, H. J. (1936). A Graphic Method for Comparing Several
#'  Sets of Measurements. *Contributions from the Laboratory of Vertebrate
#'  Genetics*, 3: 1-3.
#'
#'  Hubbs, C. L., & C. Hubbs (1953). An Improved Graphical Analysis and
#'  Comparison of Series of Samples. *Systematic Biology*, 2(2): 49-56.
#'  \doi{10.2307/sysbio/2.2.49}.
#'
#'  Simpson, G. G., Roe, A., & Lewontin, R. C. *Quantitative Zoology*.
#'  New York: Harcourt, Brace and Company, 1960.
#' @return
#'  A [ggplot2::ggplot] object.
#' @example inst/examples/ex-diceleraas.R
#' @author N. Frerebeau
#' @family plot methods
#' @docType methods
#' @aliases plot_diceleraas-method
setGeneric(
  name = "plot_diceleraas",
  def = function(object, ...) standardGeneric("plot_diceleraas")
)

## Line Plot -------------------------------------------------------------------
#' Line Plot
#'
#' Plots a rank *vs* relative abundance diagram.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each class).
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
#' @family plot methods
#' @docType methods
#' @aliases plot_rank-method
setGeneric(
  name = "plot_rank",
  def = function(object, ...) standardGeneric("plot_rank")
)

## Spot Plot -------------------------------------------------------------------
#' Spot Plot
#'
#' Plots a spot matrix.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each class).
#' @param type A [`character`] string specifying the graph to be plotted.
#'  It must be one of "`ring`" (the default) or "`plain`". Any unambiguous
#'  substring can be given.
#' @param threshold A [`function`] that takes a numeric vector as argument and
#'  returns a numeric threshold value. If `NULL` (the default), no threshold is
#'  computed.
#' @param diag A [`logical`] scalar indicating whether the diagonal of the
#'  matrix should be plotted. Only used if `object` is a symmetric matrix.
#' @param upper A [`logical`] scalar indicating whether the upper triangle of
#'  the matrix should be plotted. Only used if `object` is a symmetric matrix.
#' @param lower A [`logical`] scalar indicating whether the lower triangle of
#'  the matrix should be plotted. Only used if `object` is a symmetric matrix.
#' @param freq A [`logical`] scalar indicating whether relative frequency
#'  should be used instead of counts (absolute frequency).
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
#' @family plot methods
#' @docType methods
#' @aliases plot_spot-method
setGeneric(
  name = "plot_spot",
  def = function(object, ...) standardGeneric("plot_spot")
)

# Deprecated ===================================================================
#' Deprecated Methods
#'
#' @author N. Frerebeau
#' @docType methods
#' @name deprecate
#' @rdname deprecate
#' @keywords internal
NULL
