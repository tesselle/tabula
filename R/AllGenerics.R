# GENERIC METHODS
#' @include AllClasses.R
NULL

# Import S4 generics ===========================================================
#' @importMethodsFrom arkhe jackknife
#' @importMethodsFrom arkhe bootstrap
NULL

# Extract ======================================================================
## Mutators --------------------------------------------------------------------
#' Get or Set Parts of an Object
#'
#' Getters and setters to extract or replace parts of an object.
#' @param object,x An \R object from which to get or set element(s).
#' @param ... Currently not used.
# @param value A possible value for the element(s) of `object` (see below).
#' @return
#'  * `labels()` returns a suitable set of labels from an object for use in
#'    printing or plotting.
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

## Coerce ----------------------------------------------------------------------
#' Coerce to a Data Frame
#'
#' @param x An object.
#' @param row.names,optional Currently not used.
#' @param ... Currently not used.
#' @return
#'  A [`data.frame`].
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name data.frame
#' @rdname data.frame
NULL

# Statistic ====================================================================
#' Bootstrap Estimation
#'
#' Samples randomly from the elements of `object` with replacement.
#' @param object An \R object (typically a [DiversityIndex-class] object).
#' @param n A non-negative [`integer`] giving the number of bootstrap
#'  replications.
#' @param f A [`function`] that takes a single numeric vector (the result of
#'  `do`) as argument.
#' @return
#'  If `f` is `NULL` (the default), `bootstrap()` returns a named `numeric`
#'  vector with the following elements:
#'  \describe{
#'   \item{`original`}{The observed value of `do` applied to `object`.}
#'   \item{`mean`}{The bootstrap estimate of mean of `do`.}
#'   \item{`bias`}{The bootstrap estimate of bias of `do`.}
#'   \item{`error`}{he bootstrap estimate of standard error of `do`.}
#'  }
#'
#'  If `f` is a `function`, `bootstrap()` returns the result of `f` applied to
#'  the `n` values of `do`.
#' @example inst/examples/ex-bootstrap.R
#' @author N. Frerebeau
#' @docType methods
#' @family resampling methods
#' @name bootstrap
#' @rdname bootstrap
NULL

#' Jackknife Estimation
#'
#' @param object An \R object (typically a [DiversityIndex-class] object).
#' @param f A [`function`] that takes a single numeric vector (the leave-one-out
#'  values of `do`) as argument.
#' @return
#'  If `f` is `NULL` (the default), `jackknife()` returns a named `numeric`
#'  vector with the following elements:
#'  \describe{
#'   \item{`original`}{The observed value of `do` applied to `object`.}
#'   \item{`mean`}{The jackknife estimate of mean of `do`.}
#'   \item{`bias`}{The jackknife estimate of bias of `do`.}
#'   \item{`error`}{he jackknife estimate of standard error of `do`.}
#'  }
#'
#'  If `f` is a `function`, `jackknife()` returns the result of `f` applied to
#'  the leave-one-out values of `do`.
#' @example inst/examples/ex-jackknife.R
#' @author N. Frerebeau
#' @docType methods
#' @family resampling methods
#' @name jackknife
#' @rdname jackknife
NULL

#' Resample
#'
#' Simulates observations from a multinomial distribution.
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
#'  * `heterogeneity()` computes an heterogeneity or dominance index.
#'  * `evenness()` computes an evenness measure.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table). A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
#' @param method A [`character`] string specifying the index to be computed
#'  (see details). Any unambiguous substring can be given.
#' @param evenness A [`logical`] scalar: should an evenness measure be computed
#'  instead of an heterogeneity/dominance index?
#' @param ... Further arguments to be passed to internal methods (see below).
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
#'  either *[richness][richness()]* (weighting towards uncommon taxa) or
#'  *dominance* (weighting towards abundant taxa; Magurran 1988).
#'
#'  *Evenness* is a measure of how evenly individuals are distributed across the
#'  sample.
#' @section Heterogeneity and Evenness Measures:
#'  The following heterogeneity index and corresponding evenness measures
#'  are available (see Magurran 1988 for details):
#'  \describe{
#'   \item{`berger`}{[Berger-Parker dominance index][index_berger()].}
#'   \item{`boone`}{[Boone heterogeneity measure][index_boone()].}
#'   \item{`brillouin`}{[Brillouin diversity index][index_brillouin()].}
#'   \item{`mcintosh`}{[McIntosh dominance index][index_mcintosh()].}
#'   \item{`shannon`}{[Shannon-Wiener diversity index][index_shannon()].}
#'   \item{`simpson`}{[Simpson dominance index][index_simpson()].}
#'  }
#'
#'  The `berger`, `mcintosh` and `simpson` methods return a *dominance* index,
#'  not the reciprocal or inverse form usually adopted, so that an increase in
#'  the value of the index accompanies a decrease in diversity.
#' @return
#'  * `heterogeneity()` returns an [HeterogeneityIndex-class] object.
#'  * `evenness()` returns an [EvennessIndex-class] object.
#' @seealso [index_berger()], [index_boone()], [index_brillouin()],
#'  [index_mcintosh()], [index_shannon()], [index_simpson()]
#' @references
#'  Magurran, A. E. (1988). *Ecological Diversity and its Measurement*.
#'  Princeton, NJ: Princeton University Press.
#'  \doi{10.1007/978-94-015-7358-0}.
#'
#'  Peet, R. K. (1974). The Measurement of Species Diversity. *Annual Review of
#'  Ecology and Systematics*, 5(1), 285-307.
#'  \doi{10.1146/annurev.es.05.110174.001441}.
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

#' Berger-Parker Dominance Index
#'
#' @param x A [`numeric`] vector of count data (absolute frequencies).
#' @param na.rm A [`numeric`] scalar: should missing values (including `NaN`) be
#'  removed?
#' @param ... Currently not used.
#' @details
#'  The Berger-Parker index expresses the proportional importance of the most
#'  abundant type. This metric is highly biased by sample size and richness,
#'  moreover it does not make use of all the information available from sample.
#'
#'  This is a *dominance* index, so that an increase in the value of the index
#'  accompanies a decrease in diversity.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Berger, W. H. & Parker, F. L. (1970). Diversity of Planktonic Foraminifera
#'  in Deep-Sea Sediments. *Science*, 168(3937), 1345-1347.
#'  \doi{10.1126/science.168.3937.1345}.
#' @author N. Frerebeau
#' @family alpha diversity measures
#' @docType methods
#' @aliases index_berger-method
setGeneric(
  name = "index_berger",
  def = function(x, ...) standardGeneric("index_berger")
)

#' Boone Heterogeneity Measure
#'
#' @param x A \eqn{m \times p}{m x p} `numeric` [`matrix`] of count data
#'  (absolute frequencies, i.e. a contingency table).
#' @param j An [`integer`] giving the index of the reference type/taxa.
#'  If `NULL` (the default), the most frequent type/taxa in any assemblage will
#'  be used.
#' @param na.rm A [`numeric`] scalar: should missing values (including `NaN`) be
#'  removed?
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Boone, J. L. (1987). Defining and Measuring Midden Catchment. *American
#'  Antiquity*, 52(2), 336-45. \doi{10.2307/281785}.
#'
#'  Kintigh, K. W. (1989). Sample Size, Significance, and Measures of
#'  Diversity. In Leonard, R. D. and Jones, G. T., *Quantifying Diversity
#'  in Archaeology*. New Directions in Archaeology. Cambridge:
#'  Cambridge University Press, p. 25-36.
#' @author N. Frerebeau
#' @family alpha diversity measures
#' @docType methods
#' @aliases index_boone-method
setGeneric(
  name = "index_boone",
  def = function(x, ...) standardGeneric("index_boone")
)

#' Brillouin Diversity Index.
#'
#' @param x A [`numeric`] vector of count data (absolute frequencies).
#' @param evenness A [`numeric`] scalar: should evenness be computed?
#' @param na.rm A [`numeric`] scalar: should missing values (including `NaN`) be
#'  removed?
#' @param ... Currently not used.
#' @details
#'  The Brillouin index describes a known collection: it does not assume random
#'  sampling in an infinite population. Pielou (1975) and Laxton (1978) argues
#'  for the use of the Brillouin index in all circumstances, especially in
#'  preference to the Shannon index.
#' @note
#'  Ramanujan approximation is used for \eqn{x!} computation if \eqn{x > 170}.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Brillouin, L. (1956). *Science and information theory*. New York:
#'  Academic Press.
#'
#'  Laxton, R. R. (1978). The measure of diversity. *Journal of Theoretical
#'  Biology*, 70(1), 51-67.
#'  \doi{10.1016/0022-5193(78)90302-8}.
#'
#'  Pielou, E. C. (1975). *Ecological Diversity*. New York: Wiley.
#'  \doi{10.4319/lo.1977.22.1.0174b}
#' @author N. Frerebeau
#' @family alpha diversity measures
#' @docType methods
#' @aliases index_brillouin-method
setGeneric(
  name = "index_brillouin",
  def = function(x, ...) standardGeneric("index_brillouin")
)

#' McIntosh Dominance Index.
#'
#' @param x A [`numeric`] vector of count data (absolute frequencies).
#' @param evenness A [`numeric`] scalar: should evenness be computed?
#' @param na.rm A [`numeric`] scalar: should missing values (including `NaN`) be
#'  removed?
#' @param ... Currently not used.
#' @details
#'  The McIntosh index expresses the heterogeneity of a sample in geometric
#'  terms. It describes the sample as a point of a \eqn{S}-dimensional
#'  hypervolume and uses the Euclidean distance of this point from the origin.
#'
#'  This is a *dominance* index, so that an increase in the value of the index
#'  accompanies a decrease in diversity.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  McIntosh, R. P. (1967). An Index of Diversity and the Relation of Certain
#'  Concepts to Diversity. *Ecology*, 48(3), 392-404.
#'  \doi{10.2307/1932674}.
#' @author N. Frerebeau
#' @family alpha diversity measures
#' @docType methods
#' @aliases index_mcintosh-method
setGeneric(
  name = "index_mcintosh",
  def = function(x, ...) standardGeneric("index_mcintosh")
)

#' Shannon-Wiener Diversity Index
#'
#' @param x A [`numeric`] vector of count data (absolute frequencies).
#' @param evenness A [`numeric`] scalar: should evenness be computed?
#' @param unbiased A [`logical`] scalar: should the bias-corrected estimator be
#'  used?
#' @param ACE A [`logical`] scalar: should the ACE species richness estimator be
#'  used in the bias correction?
#' @param base A positive [`numeric`] value specifying the base with respect to
#'  which logarithms are computed.
#' @param na.rm A [`numeric`] scalar: should missing values (including `NaN`) be
#'  removed?
#' @param ... Currently not used.
#' @details
#'  The Shannon index assumes that individuals are randomly sampled from an
#'  infinite population and that all taxa are represented in the sample (it
#'  does not reflect the sample size). The main source of error arises from the
#'  failure to include all taxa in the sample: this error increases as the
#'  proportion of species discovered in the sample declines (Peet 1974,
#'  Magurran 1988). The maximum likelihood estimator (MLE) is used for the
#'  relative abundance, this is known to be negatively biased by sample size.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Peet, R. K. (1974). The Measurement of Species Diversity. *Annual Review of
#'  Ecology and Systematics*, 5(1), 285-307.
#'  \doi{10.1146/annurev.es.05.110174.001441}.
#'
#'  Magurran, A. E. (1988). *Ecological Diversity and its Measurement*.
#'  Princeton, NJ: Princeton University Press.
#'  \doi{10.1007/978-94-015-7358-0}.
#'
#'  Shannon, C. E. (1948). A Mathematical Theory of Communication. *The
#'  Bell System Technical Journal*, 27, 379-423.
#'  \doi{10.1002/j.1538-7305.1948.tb01338.x}.
#' @author N. Frerebeau
#' @family alpha diversity measures
#' @docType methods
#' @aliases index_shannon-method
setGeneric(
  name = "index_shannon",
  def = function(x, ...) standardGeneric("index_shannon")
)

#' Simpson Dominance Index
#'
#' @param x A [`numeric`] vector of count data (absolute frequencies).
#' @param evenness A [`numeric`] scalar: should evenness be computed?
#' @param unbiased A [`logical`] scalar: should the bias-corrected estimator be
#'  used?
#' @param na.rm A [`numeric`] scalar: should missing values (including `NaN`) be
#'  removed?
#' @param ... Currently not used.
#' @details
#'  The Simpson index expresses the probability that two individuals randomly
#'  picked from a finite sample belong to two different types. It can be
#'  interpreted as the weighted mean of the proportional abundances. This
#'  metric is a true probability value, it ranges from \eqn{0} (all taxa are
#'  equally present) to \eqn{1} (one taxon dominates the community completely).
#'
#'  This is a *dominance* index, so that an increase in the value of the index
#'  accompanies a decrease in diversity.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Simpson, E. H. (1949). Measurement of Diversity. *Nature*, 163(4148),
#'  688-688. \doi{10.1038/163688a0}.
#' @author N. Frerebeau
#' @family alpha diversity measures
#' @docType methods
#' @aliases index_simpson-method
setGeneric(
  name = "index_simpson",
  def = function(x, ...) standardGeneric("index_simpson")
)

## Richness --------------------------------------------------------------------
#' Richness
#'
#' @description
#'  * `richness()` computes sample richness.
#'  * `composition()` computes asymptotic species richness.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table). A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
#' @param method A [`character`] string or vector of strings specifying the
#' index to be computed (see details). Any unambiguous substring can be given.
#' @param ... Further arguments to be passed to internal methods (see below).
#' @section Details:
#'  The number of observed taxa, provides an instantly comprehensible
#'  expression of diversity. While the number of taxa within a sample
#'  is easy to ascertain, as a term, it makes little sense: some taxa
#'  may not have been seen, or there may not be a fixed number of taxa
#'  (e.g. in an open system; Peet 1974). As an alternative, *richness*
#'  (\eqn{S}) can be used for the concept of taxa number (McIntosh 1967).
#'
#'  It is not always possible to ensure that all sample sizes are equal
#'  and the number of different taxa increases with sample size and
#'  sampling effort (Magurran 1988). Then, *[rarefaction][rarefaction()]*
#'  (\eqn{E(S)}) is the number of taxa expected if all samples were of a
#'  standard size (i.e. taxa per fixed number of individuals).
#'  Rarefaction assumes that imbalances between taxa are due to sampling and
#'  not to differences in actual abundances.
#' @section Richness Measures:
#'  The following richness measures are available for count data:
#'  \describe{
#'   \item{`observed`}{Number of observed taxa/types.}
#'   \item{`margalef`}{[Margalef richness index][index_margalef()].}
#'   \item{`menhinick`}{[Menhinick richness index][index_menhinick()].}
#'  }
#'
#' @section Asymptotic Species Richness:
#'  The following measures are available for count data:
#'  \describe{
#'   \item{`ace`}{[Abundance-based Coverage Estimator][index_ace()].}
#'   \item{`chao1`}{(improved/unbiased) [Chao1 estimator][index_chao1()].}
#'   \item{`squares`}{[Squares estimator][index_squares()].}
#'  }
#'
#'  The following measures are available for replicated incidence data:
#'  \describe{
#'   \item{`ice`}{[Incidence-based Coverage Estimator][index_ice()].}
#'   \item{`chao2`}{(improved/unbiased) [Chao2 estimator][index_chao2()].}
#'  }
#' @return
#'  * `richness()` returns a [RichnessIndex-class] object.
#'  * `composition()` returns a [CompositionIndex-class] object.
#' @seealso [index_margalef()], [index_menhinick()], [index_ace()],
#'  [index_chao1()], [index_squares()], [index_ice()], [index_chao2()]
#' @references
#'  Kintigh, K. W. (1989). Sample Size, Significance, and Measures of
#'  Diversity. In Leonard, R. D. and Jones, G. T., *Quantifying Diversity
#'  in Archaeology*. New Directions in Archaeology. Cambridge:
#'  Cambridge University Press, p. 25-36.
#'
#'  Magurran, A. E. (1988). *Ecological Diversity and its Measurement*.
#'  Princeton, NJ: Princeton University Press. \doi{10.1007/978-94-015-7358-0}.
#'
#'  Magurran, A E. & Brian J. McGill (2011). *Biological Diversity:
#'  Frontiers in Measurement and Assessment*. Oxford: Oxford University Press.
#'
#'  McIntosh, R. P. (1967). An Index of Diversity and the Relation of Certain
#'  Concepts to Diversity. *Ecology*, 48(3), 392-404. \doi{10.2307/1932674}.
#'
#'  Peet, R. K. (1974). The Measurement of Species Diversity. *Annual Review of
#'  Ecology and Systematics*, 5(1), 285-307.
#'  \doi{10.1146/annurev.es.05.110174.001441}.
#' @seealso [`plot()`][plot_diversity]
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

#' Number of Observed Species
#'
#' @param x A [`numeric`] vector of count data (absolute frequencies).
#' @param na.rm A [`numeric`] scalar: should missing values (including `NaN`) be
#'  removed?
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector.
#' @family alpha diversity measures
#' @docType methods
#' @aliases observed-method
setGeneric(
  name = "observed",
  def = function(x, ...) standardGeneric("observed")
)

#' @rdname observed
#' @aliases singleton-method
setGeneric(
  name = "singleton",
  def = function(x, ...) standardGeneric("singleton")
)

#' @rdname observed
#' @aliases doubleton-method
setGeneric(
  name = "doubleton",
  def = function(x, ...) standardGeneric("doubleton")
)

#' Abundance-based Coverage Estimator
#'
#' @param x A [`numeric`] vector of count data (absolute frequencies).
#' @param k A length-one [`numeric`] vector giving the threshold between
#'  rare/infrequent and abundant/frequent species.
#' @param na.rm A [`numeric`] scalar: should missing values (including `NaN`) be
#'  removed?
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Chao, A. & Lee, S.-M. (1992). Estimating the Number of Classes via Sample
#'  Coverage. *Journal of the American Statistical Association*, 87(417),
#'  210-217. \doi{10.1080/01621459.1992.10475194}.
#' @author N. Frerebeau
#' @family alpha diversity measures
#' @docType methods
#' @aliases index_ace-method
setGeneric(
  name = "index_ace",
  def = function(x, ...) standardGeneric("index_ace")
)

#' Chao1 Estimator
#'
#' @param x A [`numeric`] vector of count data (absolute frequencies).
#' @param unbiased A [`logical`] scalar: should the bias-corrected estimator be
#'  used?
#' @param improved A [`logical`] scalar: should the improved estimator be used?
#' @param na.rm A [`numeric`] scalar: should missing values (including `NaN`) be
#'  removed?
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Chao, A. (1984). Nonparametric Estimation of the Number of Classes in a
#'  Population. *Scandinavian Journal of Statistics*, 11(4), 265-270.
#'
#'  Chiu, C.-H., Wang, Y.-T., Walther, B. A. & Chao, A. (2014). An improved
#'  nonparametric lower bound of species richness via a modified good-turing
#'  frequency formula. *Biometrics*, 70(3), 671-682.
#'  \doi{10.1111/biom.12200}.
#' @author N. Frerebeau
#' @family alpha diversity measures
#' @docType methods
#' @aliases index_chao1-method
setGeneric(
  name = "index_chao1",
  def = function(x, ...) standardGeneric("index_chao1")
)

#' Chao2 Estimator
#'
#' @param x A \eqn{m \times p}{m x p} [`matrix`] of presence/absence data
#'  (incidence).
#' @param unbiased A [`logical`] scalar: should the bias-corrected estimator be
#'  used?
#' @param improved A [`logical`] scalar: should the improved estimator be used?
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Chao, A. (1987). Estimating the Population Size for Capture-Recapture Data
#'  with Unequal Catchability. *Biometrics* 43(4), 783-791.
#'
#'  Chiu, C.-H., Wang, Y.-T., Walther, B. A. & Chao, A. (2014). An improved
#'  nonparametric lower bound of species richness via a modified good-turing
#'  frequency formula. *Biometrics*, 70(3), 671-682.
#'  \doi{10.2307/2531532}.
#' @author N. Frerebeau
#' @family alpha diversity measures
#' @docType methods
#' @aliases index_chao2-method
setGeneric(
  name = "index_chao2",
  def = function(x, ...) standardGeneric("index_chao2")
)

#' Incidence-based Coverage Estimator
#'
#' @param x A \eqn{m \times p}{m x p} [`matrix`] of presence/absence data
#'  (incidence).
#' @param k A length-one [`numeric`] vector giving the threshold between
#'  rare/infrequent and abundant/frequent species.
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Chao, A. & Chiu, C.-H. (2016). Species Richness: Estimation and Comparison.
#'  *In* Balakrishnan, N., Colton, T., Everitt, B., Piegorsch, B., Ruggeri,
#'  F. & Teugels, J. L. (Eds.), *Wiley StatsRef: Statistics Reference Online*.
#'  Chichester, UK: John Wiley & Sons, Ltd., 1-26.
#'  \doi{10.1002/9781118445112.stat03432.pub2}
#' @author N. Frerebeau
#' @family alpha diversity measures
#' @docType methods
#' @aliases index_ice-method
setGeneric(
  name = "index_ice",
  def = function(x, ...) standardGeneric("index_ice")
)

#' Margalef Richness Index
#'
#' @param x A [`numeric`] vector of count data (absolute frequencies).
#' @param na.rm A [`numeric`] scalar: should missing values (including `NaN`) be
#'  removed?
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Margalef, R. (1958). Information Theory in Ecology. *General Systems*,
#'  3, 36-71.
#' @author N. Frerebeau
#' @family alpha diversity measures
#' @docType methods
#' @aliases index_margalef-method
setGeneric(
  name = "index_margalef",
  def = function(x, ...) standardGeneric("index_margalef")
)

#' Menhinick Richness Index
#'
#' @param x A [`numeric`] vector of count data (absolute frequencies).
#' @param na.rm A [`numeric`] scalar: should missing values (including `NaN`) be
#'  removed?
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Menhinick, E. F. (1964). A Comparison of Some Species-Individuals Diversity
#'  Indices Applied to Samples of Field Insects. *Ecology*, 45(4), 859-861.
#'  \doi{10.2307/1934933}.
#' @author N. Frerebeau
#' @family alpha diversity measures
#' @docType methods
#' @aliases index_menhinick-method
setGeneric(
  name = "index_menhinick",
  def = function(x, ...) standardGeneric("index_menhinick")
)

#' Squares Estimator
#'
#' @param x A [`numeric`] vector of count data (absolute frequencies).
#' @param na.rm A [`numeric`] scalar: should missing values (including `NaN`) be
#'  removed?
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Alroy, J. (2018). Limits to Species Richness in Terrestrial Communities.
#'  *Ecology Letters*, 21(12), 1781-1789. \doi{10.1111/ele.13152}.
#' @author N. Frerebeau
#' @family alpha diversity measures
#' @docType methods
#' @aliases index_squares-method
setGeneric(
  name = "index_squares",
  def = function(x, ...) standardGeneric("index_squares")
)

## Rarefaction -----------------------------------------------------------------
#' Rarefaction
#'
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table). A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
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
#'   \item{`baxter`}{[Baxter's rarefaction][index_baxter()].}
#'   \item{`hurlbert`}{[Hurlbert's unbiased estimate][index_hurlbert()] of
#'   Sander's rarefaction.}
#'  }
#' @return
#'  A [RarefactionIndex-class] object.
#' @example inst/examples/ex-rarefaction.R
#' @seealso [index_baxter()], [index_hurlbert()], [`plot()`][plot_rarefaction]
#' @author N. Frerebeau
#' @family diversity measures
#' @docType methods
#' @aliases rarefaction-method
setGeneric(
  name = "rarefaction",
  def = function(object, ...) standardGeneric("rarefaction")
)

#' Baxter's Rarefaction
#'
#' @param x A [`numeric`] vector of count data (absolute frequencies).
#' @param sample A length-one [`numeric`] vector giving the sub-sample size.
#'  The size of sample should be smaller than total community size.
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Baxter, M. J. (2001). Methodological Issues in the Study of Assemblage
#'  Diversity. *American Antiquity*, 66(4), 715-725. \doi{10.2307/2694184}.
#' @author N. Frerebeau
#' @family alpha diversity measures
#' @docType methods
#' @aliases index_baxter-method
setGeneric(
  name = "index_baxter",
  def = function(x, ...) standardGeneric("index_baxter")
)

#' Hurlbert's Rarefaction
#'
#' Hurlbert's unbiased estimate of Sander's rarefaction.
#' @param x A [`numeric`] vector of count data (absolute frequencies).
#' @param sample A length-one [`numeric`] vector giving the sub-sample size.
#'  The size of sample should be smaller than total community size.
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Hurlbert, S. H. (1971). The Nonconcept of Species Diversity: A Critique and
#'  Alternative Parameters. *Ecology*, 52(4), 577-586.
#'  \doi{10.2307/1934145}.
#'
#'  Sander, H. L. (1968). Marine Benthic Diversity: A Comparative Study.
#'  *The American Naturalist*, 102(925), 243-282.
#' @author N. Frerebeau
#' @family alpha diversity measures
#' @docType methods
#' @aliases index_hurlbert-method
setGeneric(
  name = "index_hurlbert",
  def = function(x, ...) standardGeneric("index_hurlbert")
)

## Similarity ------------------------------------------------------------------
#' Similarity
#'
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table). A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
#' @param method A [`character`] string specifying the method to be
#'  used (see details). Any unambiguous substring can be given.
#' @param ... Currently not used.
#' @details
#'  \eqn{\beta}-diversity can be measured by addressing *similarity*
#'  between pairs of samples/cases (Brainerd-Robinson, Jaccard, Morisita-Horn
#'  and Sorenson indices).
#'
#'  Jaccard, Morisita-Horn and Sorenson indices provide a scale of similarity
#'  from \eqn{0}-\eqn{1} where \eqn{1} is perfect similarity and \eqn{0} is
#'  no similarity. The Brainerd-Robinson index is scaled between \eqn{0} and
#'  \eqn{200}.
#'
#'  \describe{
#'   \item{`brainerd`}{[Brainerd-Robinson quantitative index][index_brainerd()].}
#'   \item{`bray`}{[Sorenson quantitative index][index_bray()].}
#'   \item{`jaccard`}{[Jaccard qualitative index][index_jaccard()].}
#'   \item{`morisita`}{[Morisita-Horn quantitative index][index_morisita()].}
#'   \item{`sorenson`}{[Sorenson qualitative index][index_sorenson()].}
#'  }
#' @return
#'  A [stats::dist] object.
#' @seealso [index_binomial()], [index_brainerd()], [index_bray()],
#'  [index_jaccard()], [index_morisita()], [index_sorenson()]
#' @references
#'  Magurran, A. E. (1988). *Ecological Diversity and its Measurement*.
#'  Princeton, NJ: Princeton University Press. \doi{10.1007/978-94-015-7358-0}.
#' @example inst/examples/ex-similarity.R
#' @author N. Frerebeau
#' @family diversity measures
#' @docType methods
#' @aliases similarity-method
setGeneric(
  name = "similarity",
  def = function(object, ...) standardGeneric("similarity")
)

#' Jaccard Index
#'
#' @param x,y A [`numeric`] vector.
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Magurran, A. E. (1988). *Ecological Diversity and its Measurement*.
#'  Princeton, NJ: Princeton University Press. \doi{10.1007/978-94-015-7358-0}.
#' @author N. Frerebeau
#' @family beta diversity measures
#' @docType methods
#' @aliases index_jaccard-method
setGeneric(
  name = "index_jaccard",
  def = function(x, y, ...) standardGeneric("index_jaccard")
)

#' Sorenson Qualitative Index
#'
#' @param x,y A [`numeric`] vector.
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Magurran, A. E. (1988). *Ecological Diversity and its Measurement*.
#'  Princeton, NJ: Princeton University Press. \doi{10.1007/978-94-015-7358-0}.
#' @author N. Frerebeau
#' @family beta diversity measures
#' @docType methods
#' @aliases index_sorenson-method
setGeneric(
  name = "index_sorenson",
  def = function(x, y, ...) standardGeneric("index_sorenson")
)

#' Sorenson Quantitative Index
#'
#' Bray and Curtis modified version of the Sorenson index.
#' @param x,y A [`numeric`] vector.
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Bray, J. R. & Curtis, J. T. (1957). An Ordination of the Upland Forest
#'  Communities of Southern Wisconsin. *Ecological Monographs*, 27(4),
#'  325-349. \doi{10.2307/1942268}.
#' @author N. Frerebeau
#' @family beta diversity measures
#' @docType methods
#' @aliases index_bray-method
setGeneric(
  name = "index_bray",
  def = function(x, y, ...) standardGeneric("index_bray")
)

#' Morisita-Horn Quantitative Index
#'
#' @param x,y A [`numeric`] vector.
#' @param ... Currently not used.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Magurran, A. E. (1988). *Ecological Diversity and its Measurement*.
#'  Princeton, NJ: Princeton University Press. \doi{10.1007/978-94-015-7358-0}.
#' @author N. Frerebeau
#' @family beta diversity measures
#' @docType methods
#' @aliases index_morisita-method
setGeneric(
  name = "index_morisita",
  def = function(x, y, ...) standardGeneric("index_morisita")
)

#' Brainerd-Robinson Quantitative Index
#'
#' @param x,y A [`numeric`] vector.
#' @param ... Currently not used.
#' @details
#'  A city-block metric of similarity between pairs of samples/cases.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Brainerd, G. W. (1951). The Place of Chronological Ordering in
#'  Archaeological Analysis. *American Antiquity*, 16(04), 301-313.
#'  \doi{10.2307/276979}.
#'
#'  Robinson, W. S. (1951). A Method for Chronologically Ordering Archaeological
#'  Deposits. *American Antiquity*, 16(04), 293-301. \doi{10.2307/276978}.
#' @author N. Frerebeau
#' @family beta diversity measures
#' @docType methods
#' @aliases index_brainerd-method
setGeneric(
  name = "index_brainerd",
  def = function(x, y, ...) standardGeneric("index_brainerd")
)

## Turnover --------------------------------------------------------------------
#' Turnover
#'
#' Returns the degree of turnover in taxa composition along a gradient or
#' transect.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data or incidence data. A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
#' @param method A [`character`] string specifying the method to be
#'  used (see details). Any unambiguous substring can be given.
#' @param ... Further arguments to be passed to internal methods.
#' @details
#'  The following methods can be used to ascertain the degree of *turnover*
#'  in taxa composition along a gradient (\eqn{\beta}-diversity) on qualitative
#'  (presence/absence) data:
#'
#'  \describe{
#'   \item{`cody`}{[Cody measure][index_cody()].}
#'   \item{`routledge1`}{[Routledge first measure][index_routledge].}
#'   \item{`routledge2`}{[Routledge second measure][index_routledge].}
#'   \item{`routledge3`}{[Routledge third measure][index_routledge] (exponential
#'   form of the second measure).}
#'   \item{`whittaker`}{[Whittaker measure][index_whittaker()].}
#'   \item{`wilson`}{[Wilson measure][index_wilson()].}
#'  }
#'
#'  This assumes that the order of the matrix rows (from \eqn{1} to \eqn{n})
#'  follows the progression along the gradient/transect.
#' @return
#'  A [`numeric`] vector.
#' @seealso [index_cody()], [index_routledge1()], [index_routledge2()],
#'  [index_routledge3()], [index_whittaker()], [index_wilson()]
#' @example inst/examples/ex-turnover.R
#' @author N. Frerebeau
#' @family diversity measures
#' @docType methods
#' @aliases turnover-method
setGeneric(
  name = "turnover",
  def = function(object, ...) standardGeneric("turnover")
)

#' Cody Measure
#'
#' @param x A \eqn{m \times p}{m x p} `numeric` [`matrix`] of count data
#'  (absolute frequencies, i.e. a contingency table).
#' @param ... Currently not used.
#' @details
#'  This assumes that the order of the matrix rows (from \eqn{1} to \eqn{n})
#'  follows the progression along the gradient/transect.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Cody, M. L. (1975). Towards a theory of continental species diversity: Bird
#'  distributions over Mediterranean habitat gradients. *In* M. L. Cody &
#'  J. M. Diamond (Eds.), *Ecology and Evolution of Communities*.
#'  Cambridge, MA: Harvard University Press, p. 214-257.
#' @author N. Frerebeau
#' @family beta diversity measures
#' @docType methods
#' @aliases index_cody-method
setGeneric(
  name = "index_cody",
  def = function(x, ...) standardGeneric("index_cody")
)

#' Routledge Measures
#'
#' @param x A \eqn{m \times p}{m x p} `numeric` [`matrix`] of count data
#'  (absolute frequencies, i.e. a contingency table).
#' @param ... Currently not used.
#' @details
#'  This assumes that the order of the matrix rows (from \eqn{1} to \eqn{n})
#'  follows the progression along the gradient/transect.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Routledge, R. D. (1977). On Whittaker's Components of Diversity.
#'  *Ecology*, 58(5), 1120-1127. \doi{10.2307/1936932}.
#' @author N. Frerebeau
#' @family beta diversity measures
#' @docType methods
#' @name index_routledge
#' @rdname index_routledge
NULL

#' @rdname index_routledge
#' @aliases index_routledge1-method
setGeneric(
  name = "index_routledge1",
  def = function(x, ...) standardGeneric("index_routledge1")
)

#' @rdname index_routledge
#' @aliases index_routledge2-method
setGeneric(
  name = "index_routledge2",
  def = function(x, ...) standardGeneric("index_routledge2")
)

#' @rdname index_routledge
#' @aliases index_routledge3-method
setGeneric(
  name = "index_routledge3",
  def = function(x, ...) standardGeneric("index_routledge3")
)

#' Whittaker Measure
#'
#' @param x A \eqn{m \times p}{m x p} `numeric` [`matrix`] of count data
#'  (absolute frequencies, i.e. a contingency table).
#' @param ... Currently not used.
#' @details
#'  This assumes that the order of the matrix rows (from \eqn{1} to \eqn{n})
#'  follows the progression along the gradient/transect.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Whittaker, R. H. (1960). Vegetation of the Siskiyou Mountains, Oregon and
#'  California. *Ecological Monographs*, 30(3), 279-338.
#'  \doi{10.2307/1943563}.
#' @author N. Frerebeau
#' @family beta diversity measures
#' @docType methods
#' @aliases index_whittaker-method
setGeneric(
  name = "index_whittaker",
  def = function(x, ...) standardGeneric("index_whittaker")
)

#' Wilson Measure
#'
#' @param x A \eqn{m \times p}{m x p} `numeric` [`matrix`] of count data
#'  (absolute frequencies, i.e. a contingency table).
#' @param ... Currently not used.
#' @details
#'  This assumes that the order of the matrix rows (from \eqn{1} to \eqn{n})
#'  follows the progression along the gradient/transect.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Wilson, M. V., & Shmida, A. (1984). Measuring Beta Diversity with
#'  Presence-Absence Data. *The Journal of Ecology*, 72(3), 1055-1064.
#'  \doi{10.2307/2259551}.
#' @author N. Frerebeau
#' @family beta diversity measures
#' @docType methods
#' @aliases index_wilson-method
setGeneric(
  name = "index_wilson",
  def = function(x, ...) standardGeneric("index_wilson")
)

## Co-Occurrence ---------------------------------------------------------------
#' Co-Occurrence
#'
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table). A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
#' @param method A [`character`] string specifying the method to be
#'  used. It must be one of "`absolute`", "`relative`" or "`binomial`"
#'  (see details). Any unambiguous substring can be given.
#' @param ... Currently not used.
#' @details
#'  \describe{
#'   \item{`absolute`}{Count how many times each pairs of types occur together
#'   in at least one sample (absolute frequencies).}
#'   \item{`relative`}{Count how many times each pairs of types occur together
#'   in at least one sample (relative frequencies).}
#'   \item{`binomial`}{[Binomial co-occurrence assessment][index_binomial()].}
#'  }
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

#' Binomial Co-Occurrence Assessment
#'
#' @param x,y A [`numeric`] vector.
#' @param ... Currently not used.
#' @details
#'  This assesses the degree of co-occurrence between taxa/types within a
#'  dataset. The strongest associations are shown by large positive numbers,
#'  the strongest segregations by large negative numbers.
#'
#'  The Binomial co-occurrence assessment approximates a Z-score.
#' @return
#'  A [`numeric`] vector.
#' @references
#'  Kintigh, K. (2006). Ceramic Dating and Type Associations. In J. Hantman and
#'  R. Most (eds.), *Managing Archaeological Data: Essays in Honor of
#'  Sylvia W. Gaines*. Anthropological Research Paper, 57. Tempe, AZ: Arizona
#'  State University, p. 17-26.
#' @author N. Frerebeau
#' @family beta diversity measures
#' @docType methods
#' @aliases index_binomial-method
setGeneric(
  name = "index_binomial",
  def = function(x, y, ...) standardGeneric("index_binomial")
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
#' @seealso [`plot()`][plot_diversity], [resample()]
#' @example inst/examples/ex-plot_diversity.R
#' @author N. Frerebeau
#' @family diversity measures
#' @docType methods
#' @name simulate
#' @rdname simulate
NULL

## Diversity Test --------------------------------------------------------------
#' Diversity Test
#'
#' Compares Shannon/Simpson diversity between samples.
#' @param x,y A [`numeric`] vector, a \eqn{m \times p}{m x p} [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table). A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
#' @param adjust A [`character`] string specifying the method for
#'  adjusting \eqn{p} values (see [stats::p.adjust()]).
#' @param ... Further arguments to be passed to internal methods.
#' @return
#'  If `x` and `y` are `numeric` vectors, returns a [`list`] containing the
#'  following components:
#'  \describe{
#'   \item{`statistic`}{The value of the t-statistic.}
#'   \item{`parameter`}{The degrees of freedom for the t-statistic.}
#'   \item{`p.value`}{The p-value for the test.}
#'  }
#'
#'  If `x` is a `matrix` or a `data.frame`, returns a table of adjusted p-values
#'  in lower triangular form.
#' @example inst/examples/ex-test.R
#' @author N. Frerebeau
#' @references
#'  Magurran, A. E. (1988). *Ecological Diversity and its Measurement*.
#'  Princeton, NJ: Princeton University Press. \doi{10.1007/978-94-015-7358-0}.
#' @family statistics
#' @docType methods
#' @name test
#' @rdname test
NULL

#' @rdname test
#' @aliases test_shannon-method
setGeneric(
  name = "test_shannon",
  def = function(x, y, ...) standardGeneric("test_shannon")
)

#' @rdname test
#' @aliases test_simpson-method
setGeneric(
  name = "test_simpson",
  def = function(x, y, ...) standardGeneric("test_simpson")
)

# Plot =========================================================================
## Diversity -------------------------------------------------------------------
#' Diversity Plot
#'
#' @param x A [DiversityIndex-class] object to be plotted.
#' @param log A [`character`] string indicating which axes should be in log
#'  scale. Defaults to `x`.
#' @param col.mean,col.interval A [`character`] string specifying the
#'  color of the lines.
#' @param lty.mean,lty.interval A [`character`] string or [`numeric`]
#'  value specifying the line types.
#' @param lwd.mean,lwd.interval A non-negative [`numeric`] value specifying
#'  the line widths.
#' @param xlab,ylab A [`character`] vector giving the x and y axis labels.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x,
#'  y and z axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param ... Further [graphical parameters][graphics::par] to be passed to
#'  [graphics::points()], particularly, `cex`, `col` and `pch`.
#' @return
#'  `plot()` is called for its side-effects: it results in a graphic being
#'  displayed (invisibly returns `x`).
#' @example inst/examples/ex-plot_diversity.R
#' @author N. Frerebeau
#' @family diversity measures
#' @docType methods
#' @name plot_diversity
#' @rdname plot_diversity
NULL

#' Rarefaction Plot
#'
#' @param x A [RarefactionIndex-class] object to be plotted.
#' @param color A vector of colors (will be mapped to the rownames of `object`).
#'  If `color` is a named a named vector, then the colors will be associated
#'  with the rownames of `object`. Ignored if set to `FALSE`.
#' @param symbol A specification for the line type (will be mapped to
#'  the names of `x`). If `symbol` is a named a named vector, then the
#'  line types will be associated with the names of `x`.
#'  Ignored if set to `FALSE`.
#' @param xlab,ylab A [`character`] vector giving the x and y axis labels.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x,
#'  y and z axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param legend A [`list`] of additional arguments to be passed to
#'  [graphics::legend()]; names of the list are used as argument names.
#'  If `NULL`, no legend is displayed.
#' @param ... Further [graphical parameters][graphics::par] to be passed to
#'  [graphics::lines()].
#' @return
#'  `plot()` is called for its side-effects: it results in a graphic being
#'  displayed (invisibly returns `x`).
#' @example inst/examples/ex-rarefaction.R
#' @author N. Frerebeau
#' @family diversity measures
#' @docType methods
#' @name plot_rarefaction
#' @rdname plot_rarefaction
NULL

#' SHE Analysis
#'
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table). A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
#' @param unbiased A [`logical`] scalar: should the bias-corrected estimator be
#'  used (see [index_shannon()])?
#' @param xlab,ylab A [`character`] vector giving the x and y axis labels.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x,
#'  y and z axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param legend A [`list`] of additional arguments to be passed to
#'  [graphics::legend()]; names of the list are used as argument names.
#'  If `NULL`, no legend is displayed.
#' @param ... Further [graphical parameters][graphics::par] to be passed to
#'  [graphics::lines()] and [graphics::points()].
#' @return
#'  `she()` is called for its side-effects: it results in a graphic being
#'  displayed (invisibly returns `object`).
#' @details
#'  If samples are taken along a gradient or stratigraphic section, breaks in
#'  the curve may be used to infer discontinuities.
#'
#'  This assumes that the order of the matrix rows (from \eqn{1} to \eqn{n})
#'  follows the progression along the gradient/transect.
#' @references
#'  Buzas, M. A. & Hayek, L.-A. C. (1998). SHE analysis for biofacies
#'  identification. *Journal of Foraminiferal Research*, 1998, 28(3), 233-239.
#'
#'  Hayek, L.-A. C. & Buzas, M. A. (2010). *Surveying Natural Populations:
#'  Quantitative Tools for Assessing Biodiversity*. Second edition.
#'  New York: Columbia University Press.
#' @example inst/examples/ex-she.R
#' @author N. Frerebeau
#' @family diversity measures
#' @docType methods
#' @aliases she-method
setGeneric(
  name = "she",
  def = function(object, ...) standardGeneric("she")
)

#' Diversity Profiles
#'
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table). A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
#' @param alpha A [`numeric`] vector giving the values of the alpha parameter.
#' @param color A vector of colors (will be mapped to the rownames of `object`).
#'  If `color` is a named a named vector, then the colors will be associated
#'  with the rownames of `object`. Ignored if set to `FALSE`.
#' @param symbol A specification for the line type (will be mapped to
#'  the rownames of `object`). If `symbol` is a named a named vector, then the
#'  line types will be associated with the rownames of `object`.
#'  Ignored if set to `FALSE`.
#' @param xlab,ylab A [`character`] vector giving the x and y axis labels.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x,
#'  y and z axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param legend A [`list`] of additional arguments to be passed to
#'  [graphics::legend()]; names of the list are used as argument names.
#'  If `NULL`, no legend is displayed.
#' @param ... Further [graphical parameters][graphics::par] to be passed to
#'  [graphics::lines()]
#' @details
#'  If the profiles cross, the diversities are non-comparable across samples.
#' @return
#'  `profiles()` is called for its side-effects: it results in a graphic being
#'  displayed (invisibly returns `object`).
#' @references
#'  Tóthmérész, B. (1995). Comparison of Different Methods for Diversity
#'  Ordering. *Journal of Vegetation Science*, 6(2), 283-290.
#'  \doi{10.2307/3236223}.
#' @example inst/examples/ex-profiles.R
#' @author N. Frerebeau
#' @family diversity measures
#' @docType methods
#' @aliases profiles-method
setGeneric(
  name = "profiles",
  def = function(object, ...) standardGeneric("profiles")
)

## Matrix plot -----------------------------------------------------------------
### Spot Plot ------------------------------------------------------------------
#' Spot Plot
#'
#' Plots a spot matrix.
#' @inheritParams plot_matrix
#' @param type A [`character`] string specifying the graph to be plotted.
#'  It must be one of "`ring`" (the default) or "`plain`". Any unambiguous
#'  substring can be given.
#' @param ... Currently not used.
#' @details
#'  The spot matrix can be considered as a variant of the
#'  [Bertin diagram][plot_bertin()] where the data are first transformed to
#'  relative frequencies.
#' @return
#'  `plot_spot()` is called for its side-effects: it results in a graphic
#'  being displayed (invisibly returns `object`).
#' @note
#'  Adapted from Dan Gopstein's original
#'  [idea](https://dan.gop/articles/spot-matrix/).
#' @example inst/examples/ex-plot_spot.R
#' @author N. Frerebeau
#' @family plot methods
#' @docType methods
#' @aliases plot_spot-method
setGeneric(
  name = "plot_spot",
  def = function(object, ...) standardGeneric("plot_spot")
)

### Heatmap --------------------------------------------------------------------
#' Heatmap
#'
#' Plots a heatmap.
#' @inheritParams plot_matrix
#' @param fixed_ratio A [`logical`] scalar: should a fixed aspect ratio (1) be
#' used?
#' @param ... Currently not used.
#' @return
#'  `plot_heatmap()` is called for its side-effects: it results in a graphic
#'  being displayed (invisibly returns `object`).
#' @example inst/examples/ex-plot_heatmap.R
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
#' @inheritParams plot_matrix
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
#'  * `matrigraph()` is called for its side-effects: it results in a graphic
#'    being displayed (invisibly returns `object`).
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
#' @inheritParams plot_matrix
#' @param threshold A [`function`] that takes a numeric vector as argument and
#'  returns a numeric threshold value (see below). If `NULL` (the default), no
#'  threshold is computed. Only used if `freq` is `FALSE`.
#' @param flip A [`logical`] scalar: should `x` and `y` axis be flipped?
#'  Defaults to `TRUE`.
#' @param ... Currently not used.
#' @section Bertin Matrix:
#'  As de Falguerolles *et al.* (1997) points out:
#'  "In abstract terms, a Bertin matrix is a matrix of  displays. [...] To fix
#'  ideas, think of a data matrix, variable by case, with real valued variables.
#'  For each variable, draw a bar chart of variable value by case. High-light
#'  all bars representing a value above some sample threshold for that
#'  variable."
#' @return
#'  `plot_bertin()` is called for its side-effects: it results in a graphic
#'  being displayed (invisibly returns `object`).
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
#'  individuals for each category, i.e. a contingency table).
#' @param weights A [`logical`] scalar: should the row sums be displayed?
#' @param EPPM A [`logical`] scalar: should the EPPM be drawn?
#'  See `seriograph()`.
#' @param fill The color for filling the bars.
#' @param border The color to draw the borders.
#' @param axes A [`logical`] scalar: should axes be drawn on the plot? It will
#'  omit labels where they would abut or overlap previously drawn labels.
#' @param ... Currently not used.
#' @return
#'  `plot_ford()` is called for its side-effects: it results in a graphic
#'  being displayed (invisibly returns `object`).
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
#' @inheritParams plot_ford
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
#'  * `seriograph()` is called for its side-effects: it results in a graphic
#'    being displayed (invisibly returns `object`).
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
#'  individuals for each category, i.e. a contingency table). A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x,
#'  y and z axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param ... Further [graphical parameters][graphics::par].
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
#'  `plot_diceleraas()` is called for its side-effects: it results in a
#'  graphic being displayed (invisibly returns `object`).
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
#' Rank Plot
#'
#' Plots a rank *vs* relative abundance diagram.
#' @param object A \eqn{m \times p}{m x p} `numeric` [`matrix`] or
#'  [`data.frame`] of count data (absolute frequencies giving the number of
#'  individuals for each category, i.e. a contingency table). A [`data.frame`]
#'  will be coerced to a `numeric` `matrix` via [data.matrix()].
#' @param color A vector of colors (will be mapped to the rownames of `object`).
#'  If `color` is a named a named vector, then the colors will be associated
#'  with the rownames of `object`. Ignored if set to `FALSE`.
#' @param symbol A specification for the line type (will be mapped to
#'  the rownames of `object`). If `symbol` is a named a named vector, then the
#'  line types will be associated with the rownames of `object`.
#'  Ignored if set to `FALSE`.
#' @param log A [`character`] string which contains "`x`" if the x axis is to be
#'  logarithmic, "`y`" if the y axis is to be logarithmic and "`xy`" or "`yx`"
#'  if both axes are to be logarithmic (base 10).
# @param facet A [`logical`] scalar: should a matrix of panels defined by
#  case/sample be drawn?
#' @param xlab,ylab A [`character`] vector giving the x and y axis labels.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x,
#'  y and z axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param legend A [`list`] of additional arguments to be passed to
#'  [graphics::legend()]; names of the list are used as argument names.
#'  If `NULL`, no legend is displayed.
#' @param ... Further [graphical parameters][graphics::par].
#' @return
#'  `plot_rank()` is called for its side-effects: it results in a graphic
#'  being displayed (invisibly returns `object`).
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
