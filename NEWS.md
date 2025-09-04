# tabula 3.3.2
## Bugfixes & changes
* Fix `plot_ford()`: ignore empty rows when calculating relative frequencies.

# tabula 3.3.1
## Internals
* Fix tests.

# tabula 3.3.0
## New classes and methods
* Add `diversity()` to compute multiple alpha diversity indices.

## Enhancements
* `simulate()` gained a new `seed` argument.
* `bootstrap()` gained new `interval`, `seed` and `rare` arguments.

## Bugfixes & changes
* Deprecate `index_sorenson()` (typo).
* Deprecate `resample()`.

## Breaking changes
* Fix `bootstrap()` for `DiversityIndex` objects: revert the change introduced in v1.7.0. By default, bootstrap replicates are drawn from a multinomial distribution. This can be changed by setting the `rare` argument to `TRUE`.

# tabula 3.2.1
## Internals
* Update package metadata.

# tabula 3.2.0
## Enhancements
* Translate into French.
* `occurrence()` gained a new `method` argument.
* `plot()`, `plot_rank()`, `she()` and `profiles()` gained new `xlab` and `ylab` arguments.

## Bugfixes & changes
* Fix gradient legend width in `plot_spot()` and `plot_heatmap()`.
* Replace `similarity(x, method = "binomial")` with `occurrence(x, method = "binomial")` for consistency.
* Remove all previously deprecated methods.

# tabula 3.1.1
## New classes and methods
* Add `observed()` to get the number of observed types.
* Add `singleton()` and `doubleton()` to get the number of singleton and doubleton.

## Internals
* Use palette functions from **khroma**.

# tabula 3.1.0
## New classes and methods
* Add `she()` to plot SHE analysis.
* Add `profiles()` to plot diversity profiles.
* Add `test_shannon()` and `test_simpson()` to compare the Shannon and Simpson diversities in two samples.
* Add `index_squares()` to compute Alroy (2018) richness estimator.

## Enhancements
* `plot_heatmap()` gained a new argument to use a fixed aspect ratio.
* Improve color gradient in plot legends.

## Bugfixes & changes
* Fix ACE species richness estimator.
* Deprecate `test_diversity()`.

# tabula 3.0.1
## Enhancements
* Omit axes labels where they would abut or overlap previously drawn labels.

# tabula 3.0.0
## Bugfixes & changes
* Fix handling of missing values in heterogeneity index (#27).

## Breaking changes
* Use **graphics** instead of **ggplot2** for plotting. The internal use of **ggplot2** was badly interoperable or composable. This also reduces hard dependencies.

## Internals
* Use **tinytest** and **tinysnapshot** instead of **testthat** and **vdiffr**.

# tabula 2.0.0
## New classes and methods
* Add `plot_diceleraas()` to display a Dice-Lerass diagram.
* Add `matrigraph()` to display a heatmap highlighting the deviations from independence.
* Add `seriograph()` to display a Ford diagram highlighting the relationships between rows and columns.

## Bugfixes & changes
* `EPPM` argument of `plot_ford()` is defunct (use `seriograph()` instead).

## Breaking changes
* **Seriation methods are no longer reexported from kairos.**

# tabula 1.8.0
**Seriation methods are now reexported from kairos and will be removed in a future release.**

## New classes and methods
* Add `index_boone()`: Boone heterogeneity index.
* Add `index_baxter()`: Baxter rarefaction index.
* Add the `cantabria` dataset.

## Bugfixes & changes
* Remove all previously deprecated methods.

# tabula 1.7.0
**Seriation methods are now reexported from kairos and will be removed in a future release.**

## New classes and methods
* Add `RarefactionIndex`: S4 class that represent rarefied species richness.
* Add `bootstrap()`, `jackknife()` and `simulate()` methods to perform bootstrap and jackknife resampling and to measure diversity in simulated assemblages.
* Add `heterogeneity()`, `evenness()`, `richness()` and `composition()` methods for `matrix` and `data.frame`.
* Add `simulate()` to simulate observations from a multinomial distribution.
* Add `autoplot()` and `plot()` methods for `RarefactionIndex` objects.

## Bugfixes & changes
* `rarefaction()` now returns a `RarefactionIndex` object.
* Deprecate `bootstrap_*()`, `jackknife_*()` and `simulate_*()`.
* Deprecate `index_heterogeneity()`, `index_evenness()`, `index_richness()` and `index_composition()`.

## Breaking changes
* `turnover()` no longer accept multiple methods as argument.
* Fix `bootstrap()` method for `DiversityIndex` objects: resample with replacement instead of simulating observations from a multinomial distribution.
* No longer use classes from **arkhe**: all methods are now defined for `matrix` and `data.frame`.
* Deprecate seriation methods: `seriate_*()` and `permute()` now belong to **kairos**.

# tabula 1.6.1
## Bugfixes & changes
* Remove all previously deprecated methods.

## Breaking changes
* Remove dating methods: `*_mcd()`, `*_event()`, `*_accumulation()`, `plot_date()` now belong to **kairos**.
* Remove frequency increment test: `test_fit()` and `plot_time()` now belong to **kairos**.

# tabula 1.6.0
## New classes and methods
* Add `DateEvent`: S4 class to store the event and accumulation times of archaeological assemblages.
* Add `CompositionIndex`: S4 class that represent an asymptotic species richness.
* Add `IncrementTest`: S4 class that represent a Frequency Increment Test results.
* Add `eppm()` and `pvi()` to calculate independence statistics.
* Add `predict_event()` and `predict_accumulation` to estimate the event and accumulation dates of an assemblage.
* Add `bootstrap_*()` and `jackknife_*()` to perform bootstrap and jackknife resampling.
* Add `simulate_evenness()` and `simulate_richness()` to measure diversity in simulated assemblages.
* `seriate_average()` replaces `seriate_correspondance()`.
* `seriate_rank()` replaces `seriate_reciprocal()`.

## Bugfixes & changes
* Remove all previously deprecated methods.
* Deprecate `seriate_correspondance()`, `seriate_reciprocal()` and `refine_dates()`,
* `index_composition()` now returns a `CompositionIndex` object.
* `similarity()` now returns a `dist` object.
* `date_mcd()` now returns an `DateMCD` object.
* `test_fit()` now returns an `IncrementTest` object.
* `DiversityIndex` lost `bootstrap()` and `jackknife()` slots.
* Rename `BootCA` to `RefineCA`.
* Rename `DateModel` to `DateEvent`.
* `RefineCA` and `DateEvent` now inherit from `dimensio::CA`.

## Enhancements
* `date_mcd()`, `date_event()` and `plot_time()` gained a new `dates` argument.

## Internals
* Imports **arkhe** >= 0.3.0
* Re-export methods from **arkhe**.
* Imports **dimensio**.
* Use datasets from **folio**.
* Remove **pbapply** from suggested packages.
* Rewrite `plot_ford()` and `plot_bertin()`.

# tabula 1.5.1
## Bugfixes & changes
* CRAN pacakge check warning (r-devel) has been fixed (use a `stringsAsFactors = FALSE` default).

# tabula 1.5.0
## Bugfixes & changes
* CRAN package check error with long doubles disabled has been fixed (tested with R-hub `debian-gcc-devel-nold`).
* CRAN package check notes have been fixed.

## Internals
* Depend on **arkhe**.

# tabula 1.4.0
* Published in the [*Journal of Open Source Software*](https://doi.org/10.21105/joss.01821).

## New classes and methods
* `DiversityIndex`, `HeterogeneityIndex`, `EvennessIndex` and `RichnessIndex`: S4 classes that represent diversity index.
* `index_heterogeneity()` replaces `diversity()`.
* `index_evenness()` replaces `evenness()`.
* `index_richness()` replaces `richness()`.
* `index_composition()` allows to estimate asymptotic species richness.
* `plot_diversity()` produces a diversity *vs* sample size graph and allow to compare estimates with simulated assemblages.
* Add replacement methods for the `*Matrix` classes.

## Bugfixes & changes
* Deprecate `diversity()`, `evenness()` and `richness()`.

## Internals
* Display progress bars only if `interactive()` is `TRUE` and **pbapply** is installed.

# tabula 1.3.0
## New classes and methods
* `Matrix` S4 class is now the superclass of all matrix-like classes.
* `AbundanceMatrix` virtual S4 class is defined as the superclass of `CountMatrix`, `FrequencyMatrix` and `IncidenceMatrix`.
* `SpaceTime` S4 class represents space-time informations.
* `as_*()` coerce a `matrix` or `data.frame` to a `CountMatrix`, `FrequencyMatrix`, `IncidenceMatrix`, `OccurrenceMatrix` or `SimilarityMatrix`.
* `date_event()` replaces `dateEvent()`.
* `date_mcd()` allows Mean Ceramic Date estimation.
* `get_dates()` and `set_dates<-` allow to extract and replace chronological informations in `AbundanceMatrix` objects.
* `plot_bertin()` and `plot_ford()` replace `plotBar()`.
* `plot_date()` replaces `plotDate()`.
* `plot_date()` gained a method for `AbundanceMatrix` objects.
* `plot_heatmap()` replaces `plotMatrix()`.
* `plot_rank()` replaces `plotRank()`.
* `plot_spot()` replaces `plotSpot()`.
* `plot_time()` produces an abundance *vs.* time graph.
* `refine_dates()` and `refine_seriation()` replace `refine()`.
* `seriate_reciprocal()` and `seriate_correspondance()` replace `seriate()`.
* `test_diversity()` allows Shannon diversity test.
* `test_fit()` produces a Frequency Increment Test.

## Bugfixes & changes
* `CountMatrix`, `FrequencyMatrix` and `IncidenceMatrix` now also contain the `SpaceTime` class.
* Deprecate `plotBar()`, `plotMatrix()`, `plotRank()`, `plotSpot()`, `refine()`, `seriate()`.
* Remove `dateEvent()`.
* Empty rows/columns are removed prior to CA seriation to avoid error in `svd()`.

## Enhancements
* Add the Merzbach ceramics dataset.
* The `plot_date()` method for `DateModel` objects now allows to display an activity or a tempo plot.

## Internals
* Reduce required R version to 3.2.
* Error handling has been revised and error messages have been harmonized.
* Refer to **ggplot2** functions using `::` (stop importing the entire package).
* Use **vdiffr** to test graphical output.
* Replace `FactoMinerR::CA()` with `ca::ca()` (this avoids having to install all {FactoMineR} dependencies when only one function is used).
* Remove **dplyr** from the imported packages, move **magrittr** to suggested packages.

## Experimental
* When a `Matrix` object is first created, an identifier (UUID v4) is generated with `generate_uuid()`. This ID is preserved when coercing to another class. This makes it possible to identify objects representing the same initial data and associate them with the results of specific computations.
* `get_coordinates()` and `set_coordinates<-` allow to extract and replace spatial informations in `AbundanceMatrix` objects.
* `get_features()` allows to convert an `AbundanceMatrix` object to a `data.frame`. It is intended for compatibility with the **sf** package.

# tabula 1.2.0
## New classes and methods
* The function `dateEvent()` allows to compute chronological models as described in Bellanger and Husi (2006).
* `DateModel` this S4 class stores the results of `dateEvent()`.
* `SimilarityMatrix` this S4 class represents a (dis)similarity matrix.
* `plotDate()` method for `DateModel` objects.
* `plotSpot()` methods for `SimilarityMatrix` and `OccurrenceMatrix` objects.
* `[` operators for several classes.

## Bugfixes & changes
* `OccurrenceMatrix` now stores the number of times each pair of taxa occurs together in at least one sample.
* `similarity()` now returns an object of class `SimilarityMatrix`.
* `plotBar()` no longer add confidence interval by default.
* Remove useless accessors.

## Enhancements
* `similarity()` gained a new estimator: binomial co-occurrence assessment method (similarity between types).
* `seriate()` gained a new argument to pass a `BootCA` object.

## Internals
* Add an optional progress bars with {pbapply} in long running functions.

# tabula 1.1.0
## Bugfixes & changes
* `similarity()` now returns a symmetric matrix.

## Enhancements
* `richness()` gained new estimators:
* For abundance data: Chao1, bias-corrected Chao1, improved Chao1 and Abundance-based Coverage Estimator (ACE).
* For replicated incidence data: Chao2, bias-corrected Chao2, improved Chao2 and Incidence-based Coverage Estimator (ICE).

## Internals
* Add references in the `Description` field of the DESCRIPTION file.
* Split the documentation for alpha-diversity measures.
* Split the documentation for beta-diversity measures.

# tabula 1.0.0
* Initial version on CRAN.

## New classes and methods
* `BootCA` this S4 class stores partial bootstrap CA-based seriation results.
* `[[` operators acting on `PermutationOrder` and `BootCA` to extract parts.

## Bugfixes & changes
* `refine()` method for `CountMatrix` now use `stats::rmultinorm()` for partial bootstrap CA.

## Enhancements
* Add the Zuni and Mississippi ceramics datasets.
* `similarity()` gained a new estimator: the Brainerd-Robinson coefficient of similarity.

## Internals
* Add a vignette for matrix seriation.

# tabula 0.9.0
* Beta release.
