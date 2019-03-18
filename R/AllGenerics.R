# GENERIC METHODS
#' @include AllClasses.R coerce.R
NULL

# Date =========================================================================
#' Date archaeological assemblages
#'
#' Experimental (see note).
#' @param object A \eqn{m \times p}{m x p} matrix of count data.
#' @param dates A list of \code{\link{numeric}} dates.
#'  Dates will be matched with assemblage by names.
#' @param cutoff An \code{\link{integer}} giving the cumulative percentage of
#'  variance used to select CA factorial components for linear model fitting
#'  (see details). All compounds with a cumulative percentage of variance of
#'  less than the \code{cutoff} value will be retained.
#' @param level A length-one \code{\link{numeric}} vector giving the
#'  confidence level.
#' @param jackknife A \code{\link{logical}} scalar: should the model be checked
#'  by jackknife estimation (removing each fabric/type one at a time)?
#' @param bootstrap A \code{\link{logical}} scalar: should the model be checked
#'  by bootstrap resampling?
#' @param n A non-negative \code{\link{integer}} giving the number of partial
#'  bootstrap replications (see details).
#' @param ... Further arguments to be passed to \code{\link[FactoMineR]{CA}}.
#' @details
#'  This is an implementation of the chronological modeling method developed by
#'  Bellanger and Husi (2012, 2013).
#'
#'  This method allows the estimation of two probability densities. The
#'  first one (\emph{event date}) represents the \emph{terminus post-quem} of an
#'  archaeological assemblage: an event dated in calendar time. The second
#'  represents the "chronological profile" of the assemblage: the accumulation
#'  rate (Bellanger and Husi 2012).
#'
#'  This method - somewhat similar to that described by Poblome and Groenen
#'  2003 - is based on the adjustment of a Gaussian multiple linear regression
#'  model on the factors resulting from a correspondence analysis. This model
#'  results from the known dates of a selection of reliable contexts and allows
#'  to predict the \emph{event} dates of the remaining assemblage with a 95%
#'  confidence interval.
#'
#'  Since correspondence analysis allows the rows and columns of a contingency
#'  table to be projected in the same space (through the transition formula),
#'  it is possible to estimate the date of each fabric using the previous model.
#'  Finally, the \emph{accumulation} date of each context is defined as the
#'  mean of the fabric dates, weighted by their relative proportions in that
#'  context (akin to the \emph{Mean Ceramic Date} proposed by South 1977).
#'
#'  This method relies on strong archaeological and statistical assumptions.
#'  Use it if you know what you are doing (see references below).
#' @section Model checking:
#'  Resampling methods can be used to check the stability of the resulting
#'  model. If \code{jackknife} is \code{TRUE}, one type/fabric is removed at a
#'  time and all statistics are recalculated. In this way, one can assess
#'  whether certain type/fabric has a substantial influence on the date
#'  estimate. If \code{bootstrap} is \code{TRUE}, a large number of new
#'  bootstrap assemblages is created, with the same sample size, by resampling
#'  each of the original assemblage with replacement. Then, examination of the
#'  bootstrap statistics makes it possible to pinpoint assemblages that require
#'  further investigation.
#'
#'  The use of resampling options (\code{jackknife} and \code{bootstrap}) can
#'  lead to much longer execution times and larger output objects.
#'  To monitor the execution of these re-sampling procedures, a progress bar
#'  will automatically be displayed if the \code{\link[pbapply]{pbapply}}
#'  package is installed on your machine.
#' @note
#'  The original authors of the method did not publish the data supporting their
#'  demonstration and some elements are unclear. As such, no replication of
#'  their results is possible and this implementation should be considered
#'  \strong{EXPERIMENTAL}. It may be subject to major changes in a future
#'  release.
#' @return
#'  An object of class \linkS4class{DateModel}.
#' @references
#'  Bellanger, L. & Husi, P. (2013). Mesurer et modéliser le temps inscrit dans
#'  la matière à partir d'une source matérielle : la céramique médiévale.
#'  In \emph{Mesure et Histoire Médiévale}. Histoire ancienne et médiévale.
#'  Paris: Publication de la Sorbonne, p. 119-134.
#'
#'  Bellanger, L. & Husi, P. (2012). Statistical Tool for Dating and
#'  Interpreting Archaeological Contexts Using Pottery. \emph{Journal of
#'  Archaeological Science}, 39(4), 777-790.
#'  DOI: \href{https://doi.org/10.1016/j.jas.2011.06.031}{10.1016/j.jas.2011.06.031}.
#'
#'  Bellanger, L., Tomassone, R. & Husi, P. (2008). A Statistical Approach for
#'  Dating Archaeological Contexts. \emph{Journal of Data Science}, 6, 135-154.
#'
#'  Bellanger, L., Husi, P. & Tomassone, R. (2006). Une approche statistique
#'  pour la datation de contextes archéologiques. \emph{Revue de Statistique
#'  Appliquée}, 54(2), 65-81.
#'
#'  Bellanger, L., Husi, P. & Tomassone, R. (2006). Statistical Aspects of
#'  Pottery Quantification for the Dating of Some Archaeological Contexts.
#'  \emph{Archaeometry}, 48(1), 169-183.
#'  DOI: \href{https://doi.org/10.1111/j.1475-4754.2006.00249.x}{10.1111/j.1475-4754.2006.00249.x}.
#'
#'  Poblome, J. & Groenen, P. J. F. (2003). Constrained Correspondence Analysis
#'  for Seriation of Sagalassos Tablewares. In Doerr, M. & Apostolis, S. (eds.),
#'  \emph{The Digital Heritage of Archaeology}. Athens: Hellenic Ministry of
#'  Culture.
#'
#'  South, S. A. (1977). \emph{Method and Theory in Historical Archaeology}.
#'  New York: Academic Press.
#' @seealso \link{refine}
#' @example inst/examples/ex-dating.R
#' @author N. Frerebeau
#' @docType methods
#' @rdname date

#' @rdname date
#' @aliases dateEvent-method
setGeneric(
  name = "dateEvent",
  def = function(object, ...) standardGeneric("dateEvent")
)

# ==============================================================================
#' Heterogeneity and evenness
#'
#' @description
#'  \code{diversity} returns a diversity or dominance index.
#'  \code{evenness} returns an evenness measure.
#' @param object A \eqn{m \times p}{m x p} matrix of count data.
#' @param method A \code{\link{character}} string or vector of strings
#'  specifiying the index to be computed (see details).
#'  Any unambiguous substring can be given.
#' @param simplify A \code{\link{logical}} scalar: should the result be
#'  simplified to a matrix? The default value, \code{FALSE}, returns a list.
#' @param ... Further arguments passed to other methods.
#' @details
#'  \emph{Diversity} measurement assumes that all individuals in a specific
#'  taxa are equivalent and that all types are equally different from each
#'  other (Peet 1974). A measure of diversity can be achieved by using indices
#'  built on the relative abundance of taxa. These indices (sometimes referred
#'  to as non-parametric indices) benefit from not making assumptions about the
#'  underlying distribution of taxa abundance: they only take relative abundances
#'  of the species that are present and species richness into account.
#'  Peet (1974) refers to them as indices of \emph{heterogeneity}.
#'
#'  Diversity indices focus on one aspect of the taxa abundance and emphasize
#'  either \emph{richness} (weighting towards uncommon taxa)
#'  or dominance (weighting towards abundant taxa; Magurran 1988).
#'
#'  \emph{Evenness} is a measure of how evenly individuals are distributed
#'  across the sample.
#'
#'  The following heterogeneity index and corresponding evenness measures
#'  are available (see Magurran 1988 for details):
#'  \describe{
#'   \item{berger}{Berger-Parker dominance index. The Berger-Parker index
#'    expresses the proportional importance of the most abundant type. This
#'    metric is highly biased by sample size and richness, moreover it does not
#'    make use of all the information available from sample.}
#'   \item{brillouin}{Brillouin diversity index. The Brillouin index describes a
#'    known collection: it does not assume random sampling in an infinite
#'    population. Pielou (1975) and Laxton (1978) argues for the use of the
#'    Brillouin index in all circumstances, especially in preference to the
#'    Shannon index.}
#'   \item{mcintosh}{McIntosh dominance index. The McIntosh index expresses the
#'    heterogeneity of a sample in geometric terms. It describes the sample as a
#'    point of a \code{S}-dimensional hypervolume and uses the Euclidean
#'    distance of this point from the origin.}
#'   \item{shannon}{Shannon-Wiener diversity index. The Shannon index assumes
#'    that individuals are randomly sampled from an infinite population and that
#'    all taxa are represented in the sample (it does not reflect the
#'    sample size). The main source of error arises from the failure to include
#'    all taxa in the sample: this error increases as the proportion of species
#'    discovered in the sample declines (Peet 1974, Magurran 1988). The
#'    maximum likelihood estimator (MLE) is used for the relative abundance,
#'    this is known to be negatively biased by sample size.}
#'   \item{simpson}{Simpson dominance index for finite sample. The Simpson index
#'    expresses the probability that two individuals randomly picked from a
#'    finite sample belong to two different types. It can be interpreted as the
#'    weighted mean of the proportional abundances. This metric is a true
#'    probability value, it ranges from \code{0} (perfectly uneven) to \code{1}
#'    (perfectly even).}
#'  }
#'
#'  The \code{berger}, \code{mcintosh} and \code{simpson} methods return a
#'  \emph{dominance} index, not the reciprocal or inverse form usually adopted,
#'  so that an increase in the value of the index accompanies a decrease in
#'  diversity.
#' @return
#'  If \code{simplify} is \code{FALSE}, then \code{diversity} and
#'  \code{evenness} return a list (default), else return a matrix.
#' @note
#'  Ramanujan approximation is used for \eqn{x!} computation if \eqn{x > 170}.
#' @references
#'  Berger, W. H. & Parker, F. L. (1970). Diversity of Planktonic Foraminifera
#'  in Deep-Sea Sediments. \emph{Science}, 168(3937), 1345-1347.
#'  DOI: \href{https://doi.org/10.1126/science.168.3937.1345}{10.1126/science.168.3937.1345}.
#'
#'  Brillouin, L. (1956). \emph{Science and information theory}. New York:
#'  Academic Press.
#'
#'  Laxton, R. R. (1978). The measure of diversity. \emph{Journal of Theoretical
#'  Biology}, 70(1), 51-67.
#'  DOI: \href{https://doi.org/10.1016/0022-5193(78)90302-8}{10.1016/0022-5193(78)90302-8}.
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
#' @example inst/examples/ex-diversity.R
#' @author N. Frerebeau
#' @family alpha-diversity
#' @seealso
#'  \code{\link{turnover}}
#'  \code{\link{similarity}}
#' @docType methods
#' @rdname diversity-method
#' @aliases diversity-method
setGeneric(
  name = "diversity",
  def = function(object, ...) standardGeneric("diversity")
)

#' @rdname diversity-method
#' @aliases evenness-method
setGeneric(
  name = "evenness",
  def = function(object, ...) standardGeneric("evenness")
)

# Plot =========================================================================
#' Date plot
#'
#' Plots date estimates.
#' @param object An object of class \linkS4class{DateModel} to be plotted.
#' @param type A \code{\link{character}} string or vector of character strings
#'  indicating the modelled dates to be plotted. It must be one or both
#'  (default) of \code{event} and \code{accumulation}. Any unambiguous substring
#'  can be given.
#' @param select A \code{\link{numeric}} or \code{\link{character}} vector
#'  giving the selection of the assemblage that are drawn.
#' @param n A length-one non-negative \code{\link{numeric}} vector giving the
#'  desired length of the vector of quantiles for density computation.
# @param sort A \code{\link{character}} string indicating whether the dates
#  should be sorted. It can be one of \code{asc} or \code{dsc} (default).
#  Any unambiguous substring can be given. If \code{NULL} no sorting is
#  performed.
#' @param ... Further arguments passed to other methods.
#' @details
#'  Plots the two probability estimate density curves of
#'  archaeological assembalge dates (\emph{event} and
#'  \emph{accumulation} dates; Bellanger and Husi 2012).
#'
#'  The estimated probability density of an event date is approached by a normal
#'  distribution. The distribution of the accumulation time of each context is
#'  approached by a Gaussian mixture.
#'
#'  The \emph{event date} is plotted as a line, while the \emph{accumulation
#'  time} is shown as a grey filled area.
#' @references
#'  Bellanger, L. & Husi, P. (2012). Statistical Tool for Dating and
#'  Interpreting Archaeological Contexts Using Pottery. \emph{Journal of
#'  Archaeological Science}, 39(4), 777-790.
#'  DOI: \href{https://doi.org/10.1016/j.jas.2011.06.031}{10.1016/j.jas.2011.06.031}.
#' @author N. Frerebeau
#' @family plot
#' @seealso \link{dateEvent}
#' @docType methods
#' @rdname plotDate-method
#' @aliases plotDate-method
setGeneric(
  name = "plotDate",
  def = function(object, ...) standardGeneric("plotDate")
)
# ------------------------------------------------------------------------------
#' Bar plot
#'
#' Plots a Bertin or a Ford (battleship curve) diagram.
#' @param object An object to be plotted.
#' @param level A length-one \code{\link{numeric}} vector giving the
#'  confidence level to be drawn.
#' @param EPPM A \code{\link{logical}} scalar: should the EPPM be drawn (see
#'  details)?
#' @param center A \code{\link{logical}} scalar: should the bar plot
#'  be centered? The default, \code{TRUE}, produces a Ford diagram, otherwise it
#'  produces a Bertin diagram.
#' @param horizontal A \code{\link{logical}} scalar: should the bar plot
#'  be horizontal? The default, \code{FALSE}, means variables in rows and
#'  cases in columns (i.e. Bertin diagram). Only used if \code{center} is
#'  \code{FALSE}.
#' @param ... Further arguments passed to other methods.
#' @details
#'  If \code{EPPM} is \code{TRUE} and if a relative abundance is greater than
#'  the mean percentage of the type, the exceeding part is highlighted.
#'  This positive difference from the column mean percentage (in french "écart
#'  positif au pourcentage moyen", EPPM) represents a deviation from the
#'  situation of statistical independence. As independence can be interpreted as
#'  the absence of relationships between types and the chronological order of
#'  the assemblages, \code{EPPM} is a usefull graphical tool to explore
#'  significance of relationship between rows and columns related to
#'  \code{\link[=seriate]{seriation}}.
#' @references
#'  Bertin, J. (1977). \emph{La graphique et le traitement graphique de
#'  l'information}. Paris: Flammarion. Nouvelle Bibliothèque Scientifique.
#'
#'  Desachy, B. (2004). Le sériographe EPPM: un outil informatisé de sériation
#'  graphique pour tableaux de comptages. \emph{Revue archéologique de
#'  Picardie}, 3(1), 39-56.
#'  DOI: \href{https://doi.org/10.3406/pica.2004.2396}{10.3406/pica.2004.2396}.
#'
#'  Ford, J. A. (1962). \emph{A quantitative method for deriving cultural
#'  chronology}. Washington, DC: Pan American Union. Technical manual 1.
#' @example inst/examples/ex-plotBar.R
#' @author N. Frerebeau
#' @family plot
#' @docType methods
#' @rdname plotBar-method
#' @aliases plotBar-method seriographe
setGeneric(
  name = "plotBar",
  def = function(object, ...) standardGeneric("plotBar")
)
# ------------------------------------------------------------------------------
#' Matrix plot
#'
#' Plots a heatmap.
#' @param object An object to be plotted.
#' @param PVI A \code{\link{logical}} scalar: should the PVI be drawn instead of
#'  frequencies (see details)?
#' @param ... Further arguments passed to other methods.
#' @details
#'  If \code{PVI} is \code{FALSE}, it plots a heatmap of relative abundances
#'  (frequency), otherwise percentages of the independence value are drawn (in
#'  french, "pourcentages de valeur d'indépendance", PVI).
#'
#'  \code{PVI} is calculated for each cell as the percentage to the column
#'  theoretical independence value: \code{PVI} greater than \eqn{1} represent
#'  positive deviations from the independance, whereas \code{PVI} smaller than
#'  \eqn{1} represent negative deviations (Desachy 2004).
#'
#'  The \code{PVI} matrix allows to explore deviations from independence
#'  (an intuitive graphical approach to \eqn{\chi^2}{Chi-squared}),
#'  in such a way that a high-contrast matrix has quite significant deviations,
#'  with a low risk of being due to randomness (Desachy 2004).
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
#' @aliases plotMatrix-method matrigraphe
setGeneric(
  name = "plotMatrix",
  def = function(object, ...) standardGeneric("plotMatrix")
)
# ------------------------------------------------------------------------------
#' Rank vs abundance plot
#'
#' Plots a rank \emph{vs} relative abundance diagram.
#' @param object An object to be plotted.
#' @param facet A \code{\link{logical}} scalar: should a matrix of panels
#'  defined by case/sample be drawn?
#' @param log A \code{\link{character}} string which contains "\code{x}" if the
#' x axis is to be logarithmic, "\code{y}" if the y axis is to be logarithmic
#' and "\code{xy}" or "\code{yx}" if both axes are to be logarithmic (base 10).
#' @param ... Further arguments passed to other methods.
#' @details
#'  Note that rows are scaled to 0-1 (frequencies).
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
#' Plots a spot matrix.
#' @param object An object to be plotted.
#' @param threshold A \code{\link{function}} that takes a numeric vector as
#'  argument and returns a single numeric value (see details).
#'  If \code{NULL}, no threshold is computed.
#' @param ... Further arguments passed to other methods.
#' @details
#'  Note that rows are scaled to 0-1 (frequencies).
#' @note Adapted from Dan Gopstein's original
#'  \href{https://dgopstein.github.io/articles/spot-matrix/}{spot matrix}.
#'  Credit should be given to him.
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
#'  \code{richness} returns sample richness.
#'  \code{rarefaction} returns Hurlbert's unbiaised estimate of Sander's
#'  rarefaction.
#' @param object A \eqn{m \times p}{m x p} matrix of count data.
#' @param method A \code{\link{character}} string or vector of strings
#'  specifiying the index to be computed (see details).
#'  Any unambiguous substring can be given.
#' @param unbiased A \code{\link{logical}} scalar. Should the bias-corrected
#'  estimator be used? Only used with "\code{chao1}" or "\code{chao2}"
#'  (improved) estimator.
#' @param sample A length-one \code{\link{numeric}} vector giving the sub-sample
#'  size.
#' @param k A length-one \code{\link{numeric}} vector giving the threshold
#'  between rare/infrequent and abundant/frequent species. Only used if
#'  \code{method} is "\code{ace}" or "\code{ice}".
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
#'  sampling effort (Magurran 1988). Then, \emph{rarefaction} (\eqn{E(S)}) is
#'  the number of taxa expected if all samples were of a standard size (i.e.
#'  taxa per fixed number of individuals). Rarefaction assumes that imbalances
#'  between taxa are due to sampling and not to differences in actual
#'  abundances.
#'
#'  The following richness measures are available for count data:
#'  \describe{
#'   \item{ace}{Abundance-based Coverage Estimator.}
#'   \item{chao1}{Chao1 estimator.}
#'   \item{chao1i}{Improved Chao1 estimator.}
#'   \item{margalef}{Margalef richness index.}
#'   \item{menhinick}{Menhinick richness index.}
#'   \item{none}{Returns the number of observed taxa/types.}
#'  }
#'
#'  The following richness measures are available for replicated incidence data:
#'  \describe{
#'   \item{ice}{Incidence-based Coverage Estimator.}
#'   \item{chao2}{Chao2 estimator.}
#'   \item{chao2i}{Improved Chao2 estimator.}
#'  }
#' @return
#'  \code{rarefaction} returns a numeric vector.
#'
#'  If \code{simplify} is \code{FALSE}, then \code{richness} method returns a
#'  list (default), else returns a matrix (for \code{CountMatrix}) or a
#'  a numeric vector (for \code{IncidenceMatrix}).
#' @references
#'  Chao, A. (1984). Nonparametric Estimation of the Number of Classes in a
#'  Population. \emph{Scandinavian Journal of Statistics}, 11(4), 265-270.
#'
#'  Chao, A. (1987). Estimating the Population Size for Capture-Recapture Data
#'  with Unequal Catchability. \emph{Biometrics} 43(4), 783-791.
#'  DOI: \href{https://doi.org/10.2307/2531532}{10.2307/2531532}.
#'
#'  Chao, A. & Chiu, C.-H. (2016). Species Richness: Estimation and Comparison.
#'  \emph{In} Balakrishnan, N., Colton, T., Everitt, B., Piegorsch, B., Ruggeri,
#'  F. & Teugels, J. L. (Eds.), \emph{Wiley StatsRef: Statistics Reference Online}.
#'  Chichester, UK: John Wiley & Sons, Ltd., 1-26.
#'  DOI: \href{https://doi.org/10.1002/9781118445112.stat03432.pub2}{10.1002/9781118445112.stat03432.pub2}
#'
#'  Chao, A. & Lee, S.-M. (1992). Estimating the Number of Classes via Sample
#'  Coverage. \emph{Journal of the American Statistical Association}, 87(417),
#'  210-217.
#'  DOI: \href{https://doi.org/10.1080/01621459.1992.10475194}{10.1080/01621459.1992.10475194}.
#'
#'  Chiu, C.-H., Wang, Y.-T., Walther, B. A. & Chao, A. (2014). An improved
#'  nonparametric lower bound of species richness via a modified good-turing
#'  frequency formula. \emph{Biometrics}, 70(3), 671-682.
#'  DOI: \href{https://doi.org/10.1111/biom.12200}{10.1111/biom.12200}.
#'
#'  Hurlbert, S. H. (1971). The Nonconcept of Species Diversity: A Critique and
#'  Alternative Parameters. \emph{Ecology}, 52(4), 577-586.
#'  DOI: \href{https://doi.org/10.2307/1934145}{10.2307/1934145}.
#'
#'  Magurran, A. E. (1988). \emph{Ecological Diversity and its Measurement}.
#'  Princeton, NJ: Princeton University Press.
#'  DOI:\href{https://doi.org/10.1007/978-94-015-7358-0}{10.1007/978-94-015-7358-0}.
#'
#'  Margalef, R. (1958). Information Theory in Ecology. \emph{General Systems},
#'  3, 36-71.
#'
#'  Menhinick, E. F. (1964). A Comparison of Some Species-Individuals Diversity
#'  Indices Applied to Samples of Field Insects. \emph{Ecology}, 45(4), 859-861.
#'  DOI: \href{https://doi.org/10.2307/1934933}{10.2307/1934933}.
#'
#'  McIntosh, R. P. (1967). An Index of Diversity and the Relation of Certain
#'  Concepts to Diversity. \emph{Ecology}, 48(3), 392-404.
#'  DOI: \href{https://doi.org/10.2307/1932674}{10.2307/1932674}.
#'
#'  Sander, H. L. (1968). Marine Benthic Diversity: A Comparative Study.
#'  \emph{The American Naturalist}, 102(925), 243-282.
#' @example inst/examples/ex-richness.R
#' @author N. Frerebeau
#' @family alpha-diversity
#' @seealso
#'  \code{\link{turnover}}
#'  \code{\link{similarity}}
#' @docType methods
#' @rdname richness-method
#' @aliases richness-method
setGeneric(
  name = "richness",
  def = function(object, ...) standardGeneric("richness")
)

#' @rdname richness-method
#' @aliases rarefaction-method
setGeneric(
  name = "rarefaction",
  def = function(object, ...) standardGeneric("rarefaction")
)

# ==============================================================================
#' Matrix seriation
#'
#' @description
#'  \code{seriate} computes a permutation order for rows and/or columns.
#'
#'  \code{permute} rearranges a data matrix according to a permutation order.
#'
#'  \code{refine} performs a partial bootstrap correspondance analysis
#'  seriation refinement.
#' @param object An \eqn{m \times p}{m x p} data matrix.
#' @param subset A \linkS4class{BootCA} object giving the subset of
#'  \code{object} to be used.
#' @param order A \linkS4class{PermutationOrder} object giving the permutation
#'  order for rows and columns.
#' @param method A \code{\link{character}} string specifiying the method to be
#'  used. This must be one of "\code{reciprocal}", "\code{correspondance}"
#'  (see details). Any unambiguous substring can be given.
#' @param EPPM A \code{\link{logical}} scalar: should the seriation be computed
#'  on EPPM instead of raw data?
#' @param margin A \code{\link{numeric}} vector giving the subscripts which the
#'  rearrangement will be applied over: \code{1} indicates rows, \code{2}
#'  indicates columns, \code{c(1, 2)} indicates rows then columns,
#'  \code{c(2, 1)} indicates columns then rows.
#' @param stop A length-one \code{\link{numeric}} vector giving the stopping rule
#'  (i.e. maximum number of iterations) to avoid infinite loop.
#' @param cutoff A function that takes a numeric vector as argument and returns
#'  a single numeric value (see details).
#' @param n A non-negative \code{\link{integer}} giving the number of partial
#'  bootstrap replications (see details).
#' @param axes A \code{\link{numeric}} vector giving the subscripts of the CA
#'  axes to use (see details).
#' @param ... Further arguments passed to other methods.
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
#'  approximate the required pattern. The resulting order is infered
#'  to be chronological.
#'
#'  The following seriation methods are available:
#'  \describe{
#'   \item{correspondance}{Correspondance analysis-based seriation.
#'   Correspondance analysis (CA) is an effective method for the seriation of
#'   archaeological assemblages. The order of the rows and columns is given by
#'   the coordinates along one dimension of the CA space, assumed to account
#'   for temporal variation. The direction of temporal change within the
#'   correspondance analysis space is arbitrary: additional information is
#'   needed to determine the actual order in time.}
#'   \item{reciprocal}{Reciprocal ranking (incidence data) or averaging
#'   (frequency data) seriation. These procedures iteratively rearrange rows
#'   and/or columns according to their weighted rank in the data matrix until
#'   convergence. Note that this procedure could enter into an infinite loop.
#'   If no convergence is reached before the maximum number of iterations, it
#'   stops with a warning.}
#'  }
#' @section CA seriation refining:
#'  \code{refine} allows to identify samples that are subject to sampling error
#'  or samples that have underlying structural relationships and might be
#'  influencing the ordering along the CA space.
#'
#'  This relies on a partial bootstrap approach to CA-based seriation where each
#'  sample is replicated \code{n} times. The maximum dimension length of
#'  the convex hull around the sample point cloud allows to remove samples for
#'  a given \code{cutoff} value.
#'
#'  According to Peebles and Schachner (2012), "[this] point removal procedure
#'  [results in] a reduced dataset where the position of individuals within the
#'  CA are highly stable and which produces an ordering consistend with the
#'  assumptions of frequency seriation."
#'
#'  If the results of \code{\link{refine}} is used as an input argument in
#'  \code{seriate}, a correspondance analysis is performed on the subset of
#'  \code{object} which matches the samples to be kept. Then excluded samples
#'  are projected onto the dimensions of the CA coordinate space using the row
#'  transition formulae. Finally, row coordinates onto the first dimension
#'  give the seriation order.
#'
#'  To monitor the execution of these re-sampling procedure, a progress bar
#'  will automatically be displayed if the \code{\link[pbapply]{pbapply}}
#'  package is installed on your machine.
#' @return
#'  \code{seriate} returns a \linkS4class{PermutationOrder} object.
#'
#'  \code{permute} returns either a
#'  \linkS4class{CountMatrix}, \linkS4class{FrequencyMatrix} or
#'  \linkS4class{IncidenceMatrix} (the same as \code{object}).
#'
#'  \code{refine} returns a \linkS4class{BootCA} object containing the subscript
#'  of samples to be kept (i.e. samples with maximum dimension length of the
#'  convex hull smaller than the cutoff value).
#' @references
#'  Desachy, B. (2004). Le sériographe EPPM: un outil informatisé de sériation
#'  graphique pour tableaux de comptages. \emph{Revue archéologique de
#'  Picardie}, 3(1), 39-56.
#'  DOI: \href{https://doi.org/10.3406/pica.2004.2396}{10.3406/pica.2004.2396}.
#'
#'  Dunnell, R. C. (1970). Seriation Method and Its Evaluation. \emph{American
#'  Antiquity}, 35(03), 305-319.
#'  DOI: \href{https://doi.org/10.2307/278341}{10.2307/278341}.
#'
#'  Ihm, P. (2005). A Contribution to the History of Seriation in Archaeology.
#'  In C. Weihs & W. Gaul (Eds.), \emph{Classification: The Ubiquitous Challenge}
#'  (p. 307-316). Berlin Heidelberg: Springer.
#'  DOI: \href{https://doi.org/10.1007/3-540-28084-7_34}{10.1007/3-540-28084-7_34}.
#'
#'  Peeples, M. A., & Schachner, G. (2012). Refining correspondence
#'  analysis-based ceramic seriation of regional data sets. \emph{Journal of
#'  Archaeological Science}, 39(8), 2818-2827.
#'  DOI: \href{https://doi.org/10.1016/j.jas.2012.04.040}{10.1016/j.jas.2012.04.040}.
#' @seealso \link{refine} \link[FactoMineR]{CA}
#' @example inst/examples/ex-seriation.R
#' @author N. Frerebeau
#' @docType methods
#' @name seriation
#' @rdname seriation
NULL

#' @rdname seriation
#' @aliases seriate-method
setGeneric(
  name = "seriate",
  def = function(object, subset, ...) standardGeneric("seriate")
)

#' @rdname seriation
#' @aliases permute-method
setGeneric(
  name = "permute",
  def = function(object, order, ...) standardGeneric("permute")
)
#' @rdname seriation
#' @aliases refine-method
setGeneric(
  name = "refine",
  def = function(object, ...) standardGeneric("refine")
)

# ==============================================================================
#' Similarity
#'
#' \code{similarity} returns a similarity matrix.
#' @param object A \eqn{m \times p}{m x p} matrix of count data.
#' @param method A \code{\link{character}} string specifiying the method to be
#'  used (see details). Any unambiguous substring can be given.
#' @param ... Further arguments passed to other methods.
#' @details
#'  \eqn{\beta}-diversity can be measured by addressing \emph{similarity}
#'  between pairs of samples/cases (Brainerd-Robinson, Jaccard, Morisita-Horn
#'  and Sorenson indices). Similarity bewteen pairs of taxa/types can be
#'  measured by assessing the degree of co-occurrence (binomial co-occurrence).
#'
#'  Jaccard, Morisita-Horn and Sorenson indices provide a scale of similarity
#'  from \code{0}-\code{1} where \code{1} is perfect similarity and \code{0} is
#'  no similarity. The Brainerd-Robinson index is scaled between \code{0} and
#'  \code{200}. The Binomial co-occurrence assessment approximates a Z-score.
#'  \describe{
#'   \item{binomial}{Binomial co-occurrence assessment. This assesses the degree
#'   of co-occurrence between taxa/types within a dataset. The strongest
#'   associations are shown by large positive numbers, the strongest
#'   segregations by large negative numbers.}
#'   \item{brainerd}{Brainerd-Robinson quantitative index. This is a city-block
#'   metric of similarity between pairs of samples/cases.}
#'   \item{bray}{Sorenson quantitative index (Bray and Curtis modified version
#'   of the Sorenson index).}
#'   \item{jaccard}{Jaccard qualitative index.}
#'   \item{morisita}{Morisita-Horn quantitative index.}
#'   \item{sorenson}{Sorenson qualitative index.}
#'  }
#' @return
#'  \code{similarity} returns a \eqn{m \times m}{m x m} symetric matrix.
#' @references
#'  Brainerd, G. W. (1951). The Place of Chronological Ordering in
#'  Archaeological Analysis. \emph{American Antiquity}, 16(04), 301-313.
#'  DOI: \href{https://doi.org/10.2307/276979}{10.2307/276979}.
#'
#'  Bray, J. R. & Curtis, J. T. (1957). An Ordination of the Upland Forest
#'  Communities of Southern Wisconsin. \emph{Ecological Monographs}, 27(4),
#'  325-349. DOI: \href{https://doi.org/10.2307/1942268}{10.2307/1942268}.
#'
#'  Kintigh, K. (2006). Ceramic Dating and Type Associations. In J. Hantman and
#'  R. Most (eds.), \emph{Managing Archaeological Data: Essays in Honor of
#'  Sylvia W. Gaines}. Anthropological Research Paper, 57. Tempe, AZ: Arizona
#'  State University, p. 17-26.
#'
#'  Magurran, A. E. (1988). \emph{Ecological Diversity and its Measurement}.
#'  Princeton, NJ: Princeton University Press.
#'  DOI:\href{https://doi.org/10.1007/978-94-015-7358-0}{10.1007/978-94-015-7358-0}.
#'
#'  Robinson, W. S. (1951). A Method for Chronologically Ordering Archaeological
#'  Deposits. \emph{American Antiquity}, 16(04), 293-301.
#'  DOI: \href{https://doi.org/10.2307/276978}{10.2307/276978}.
#' @example inst/examples/ex-similarity.R
#' @author N. Frerebeau
#' @family beta-diversity
#' @seealso
#'  \code{\link{richness}}
#'  \code{\link{rarefaction}}
#'  \code{\link{diversity}}
#'  \code{\link{evenness}}
#' @docType methods
#' @rdname similarity-method
#' @aliases similarity-method
setGeneric(
  name = "similarity",
  def = function(object, ...) standardGeneric("similarity")
)

# ==============================================================================
#' Turnover
#'
#' Returns the degree of turnover in taxa composition along a grandient or
#' transect.
#' @param object A \eqn{m \times p}{m x p} matrix of count data.
#' @param method A \code{\link{character}} string specifiying the method to be
#'  used (see details). Any unambiguous substring can be given.
#' @param simplify A \code{\link{logical}} scalar: should the result be
#'  simplified to a matrix? The default value, \code{FALSE}, returns a list.
#' @param ... Further arguments passed to other methods.
#' @details
#'  The following methods can be used to acertain the degree of \emph{turnover}
#'  in taxa composition along a gradient (\eqn{\beta}-diversity) on qualitative
#'  (presence/absence) data. This assumes that the order of the matrix rows
#'  (from 1 to \eqn{n}) follows the progression along the gradient/transect.
#'
#'  \describe{
#'   \item{whittaker}{Whittaker measure.}
#'   \item{cody}{Cody measure.}
#'   \item{routledge1}{Routledge first measure.}
#'   \item{routledge2}{Routledge second measure.}
#'   \item{routledge3}{Routledge third measure. This is the exponential form of
#'   the second measure.}
#'   \item{wilson}{Wilson measure.}
#'  }
#' @return
#'  If \code{simplify} is \code{FALSE}, returns a list (default), else returns
#'  a matrix.
#' @references
#'  Cody, M. L. (1975). Towards a theory of continental species diversity: Bird
#'  distributions over Mediterranean habitat gradients. \emph{In} M. L. Cody &
#'  J. M. Diamond (Eds.), \emph{Ecology and Evolution of Communities}.
#'  Cambridge, MA: Harvard University Press, p. 214–257.
#'
#'  Routledge, R. D. (1977). On Whittaker’s Components of Diversity.
#'  \emph{Ecology}, 58(5), 1120-1127.
#'  DOI: \href{https://doi.org/10.2307/1936932}{10.2307/1936932}.
#'
#'  Whittaker, R. H. (1960). Vegetation of the Siskiyou Mountains, Oregon and
#'  California. \emph{Ecological Monographs}, 30(3), 279-338.
#'  DOI: \href{https://doi.org/10.2307/1943563}{10.2307/1943563}.
#'
#'  Wilson, M. V., & Shmida, A. (1984). Measuring Beta Diversity with
#'  Presence-Absence Data. \emph{The Journal of Ecology}, 72(3), 1055-1064.
#'  DOI: \href{https://doi.org/10.2307/2259551}{10.2307/2259551}.
#' @example inst/examples/ex-turnover.R
#' @author N. Frerebeau
#' @family beta-diversity
#' @seealso
#'  \code{\link{richness}}
#'  \code{\link{rarefaction}}
#'  \code{\link{diversity}}
#'  \code{\link{evenness}}
#' @docType methods
#' @rdname turnover-method
#' @aliases turnover-method
setGeneric(
  name = "turnover",
  def = function(object, ...) standardGeneric("turnover")
)
