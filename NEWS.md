## tabula 1.5.0
### Internals
* Depend on {arkhe}.

### Bugfixes & changes
* CRAN package check error with long doubles disabled has been fixed (tested with R-hub `debian-gcc-devel-nold`).
* CRAN package check notes have been fixed.

## tabula 1.4.0 (release date: 2019-11-25)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3552904.svg)](https://doi.org/10.5281/zenodo.3552904)

* Published in the [*Journal of Open Source Software*](https://doi.org/10.21105/joss.01821).

### New classes and methods
* `DiversityIndex`, `HeterogeneityIndex`, `EvennessIndex` and `RichnessIndex` S4 classes represent diversity index.
* `index_heterogeneity()` replaces `diversity()`.
* `index_evenness()` replaces `evenness()`.
* `index_richness()` replaces `richness()`.
* `index_composition()` allows to estimate asymptotic species richness.
* `plot_diversity()` produces a diversity *vs* sample size graph and allow to compare estimates with simulated assemblages.
* Add replacement methods for the `*Matrix` classes.

### Bugfixes & changes
* Deprecate `diversity()`, `evenness()` and `richness()`.

### Internals
* Display progress bars only if `interactive()` is `TRUE` and {pbapply} is installed.

## tabula 1.3.0 (CRAN release date: 2019-09-20)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3455385.svg)](https://doi.org/10.5281/zenodo.3455385)

### New classes and methods
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

### Bugfixes & changes
* `CountMatrix`, `FrequencyMatrix` and `IncidenceMatrix` now also contain the `SpaceTime` class.
* Deprecate `plotBar()`, `plotMatrix()`, `plotRank()`, `plotSpot()`, `refine()`, `seriate()`.
* Remove `dateEvent()`.
* Empty rows/columns are removed prior to CA seriation to avoid error in `svd()`.

### Enhancements
* Add the Merzbach ceramics dataset.
* The `plot_date()` method for `DateModel` objects now allows to display an activity or a tempo plot.

### Internals
* Reduce required R version to 3.2.
* Error handling has been revised and error messages have been harmonized.
* Refer to {ggplot2} functions using `::` (stop importing the entire package).
* Use {vdiffr} to test graphical output.
* Replace `FactoMinerR::CA()` with `ca::ca()` (this avoids having to install all {FactoMineR} dependencies when only one function is used).
* Remove {dplyr} from the imported packages, move {magrittr} to suggested packages.

### Experimental
* When a `Matrix` object is first created, an identifier (UUID v4) is generated with `generate_uuid()`. This ID is preserved when coercing to another class. This makes it possible to identify objects representing the same initial data and associate them with the results of specific computations.
* `get_coordinates()` and `set_coordinates<-` allow to extract and replace spatial informations in `AbundanceMatrix` objects.
* `get_features()` allows to convert an `AbundanceMatrix` object to a `data.frame`. It is intended for compatibility with the {sf} package.

## tabula 1.2.0 (CRAN release date: 2019-03-20)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2600844.svg)](https://doi.org/10.5281/zenodo.2600844)

### New classes and methods
* The function `dateEvent()` allows to compute chronological models as described in Bellanger and Husi (2006).
* `DateModel` this S4 class stores the results of `dateEvent()`.
* `SimilarityMatrix` this S4 class represents a (dis)similarity matrix.
* `plotDate()` method for `DateModel` objects.
* `plotSpot()` methods for `SimilarityMatrix` and `OccurrenceMatrix` objects.
* `[` operators for several classes.

### Bugfixes & changes
* `OccurrenceMatrix` now stores the number of times each pair of taxa occurs together in at least one sample.
* `similarity()` now returns an object of class `SimilarityMatrix`.
* `plotBar()` no longer add confidence interval by default.
* Remove useless accessors.

### Enhancements
* `similarity()` gained a new estimator: binomial co-occurrence assessment method (similarity between types).
* `seriate()` gained a new argument to pass a `BootCA` object.

### Internals
* Add an optional progress bars with {pbapply} in long running functions.

## tabula 1.1.0 (CRAN release date: 2018-12-30)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2529084.svg)](https://doi.org/10.5281/zenodo.2529084)

### Bugfixes & changes
* `similarity()` now returns a symmetric matrix.

### Enhancements
* `richness()` gained new estimators:
* For abundance data: Chao1, bias-corrected Chao1, improved Chao1 and Abundance-based Coverage Estimator (ACE).
* For replicated incidence data: Chao2, bias-corrected Chao2, improved Chao2 and Incidence-based Coverage Estimator (ICE).

### Internals
* Add references in the `Description` field of the DESCRIPTION file.
* Split the documentation for alpha-diversity measures.
* Split the documentation for beta-diversity measures.

## tabula 1.0.0 (CRAN release date: 2018-12-03)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1881131.svg)](https://doi.org/10.5281/zenodo.1881131)

* Initial version on CRAN.

### New classes and methods
* `BootCA` this S4 class stores partial bootstrap CA-based seriation results.
* `[[` operators acting on `PermutationOrder` and `BootCA` to extract parts.

### Bugfixes & changes
* `refine()` method for `CountMatrix` now use `stats::rmultinorm()` for partial bootstrap CA.

### Enhancements
* Add the Zuni and Mississippi ceramics datasets.
* `similarity()` gained a new estimator: the Brainerd-Robinson coefficient of similarity.

### Internals
* Add a vignette for matrix seriation.

## tabula 0.9.0 (release date: 2018-11-16)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1489945.svg)](https://doi.org/10.5281/zenodo.1489945)

* Beta release.
