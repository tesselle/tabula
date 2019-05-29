




<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

## tabula 1.2.0.9000 (2019-05-29)

### New classes and methods

  - `Matrix` this S4 class is now the superclass of all matrix-like
    classes.
  - `AbundanceMatrix` this virtual S4 class is defined as the superclass
    of `CountMatrix`, `FrequencyMatrix` and `IncidenceMatrix`.
  - `SpaceTime` this S4 class reprensents space-time informations.
  - `plotDate()` gained a method for `AbundanceMatrix` objects.
  - The functions `getDates()`, `setDates<-`, `getCoordinates()` and
    `setCoordinates<-` allow to extract and replace chronological and
    spatial informations in `AbundanceMatrix` objects.

### Bugfixes & changes

  - `CountMatrix`, `FrequencyMatrix` and `IncidenceMatrix` now also
    contain the `SpaceTime` class.
  - The `plotDate()` method for `DateModel` objects now allows to
    display an activity or tempo plot.

### Internals

  - Reduce required R version to 3.3.
  - Error handling has been revised and error messages have been
    harmonized.

## tabula v1.2.0 (release date: 2019-03-20)

### New classes and methods

  - The function `dateEvent()` allows to compute chronological models as
    described in Belanger and Husi (2006).
  - `DateModel` this S4 class stores the results of `dateEvent()`.
  - `SimilarityMatrix` this S4 class represents a (dis)similarity
    matrix.
  - `plotDate()` method for `DateModel` objects.
  - `plotSpot()` methods for `SimilarityMatrix` and `OccurrenceMatrix`
    objects.
  - `[` operators for several classes.

### Bugfixes & changes

  - `OccurrenceMatrix` now stores the number of times each pair of taxa
    occurs together in at least one sample.
  - `similarity()` now returns an object of class `SimilarityMatrix`.
  - `plotBar()` no longer add confidence interval by default.
  - Deprecate useless accessors.

### Enhancements

  - The function `similarity()` gained a new estimator: binomial
    co-occurrence assessment method (similarity between types).
  - The function `seriate()` gained a new argument to pass a `BootCA`
    object.

### Internals

  - Add an optional progress bars with `{pbapply}` in long running
    functions.

## tabula v1.1.0 (release date: 2018-12-30)

### Bugfixes & changes

  - `similarity()` now returns a symmetric matrix.

### Enhancements

  - The function `richness()` gained new estimators:
      - For abundance data: Chao1, bias-corrected Chao1, improved Chao1
        and Abundance-based Coverage Estimator (ACE).
      - For replicated incidence data: Chao2, bias-corrected Chao2,
        improved Chao2 and Incidence-based Coverage Estimator (ICE).

### Internals

  - Add references in the `Description` field of the DESCRIPTION file.
  - Split the documentation for alpha-diversity measures.
  - Split the documentation for beta-diversity measures.

## tabula v1.0.0 (release date: 2018-12-03)

  - Initial version on CRAN

### New classes and methods

  - `BootCA` this S4 class stores partial bootstrap CA-based seriation
    results.
  - `[[` operators acting on `PermutationOrder` and `BootCA` to extract
    parts.

### Bugfixes & changes

  - `refine()` method for `CountMatrix` now use `stats::rmultinorm()`
    for partial bootstrap CA.

### Enhancements

  - Add the Zuni and Mississippi ceramics datasets.
  - The function `similarity()` gained a new estimator: the
    Brainerd-Robinson coefficient of similarity.

### Internals

  - Add a vignette for matrix seriation.

## tabula v0.9.0 (release date: 2018-11-16)

  - First release.
