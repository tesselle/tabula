
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabula <img width=120px src="man/figures/logo.png" align="right" />

[![Appveyor build
status](https://ci.appveyor.com/api/projects/status/t1qmnis4ekqwslgs/branch/master?svg=true)](https://ci.appveyor.com/project/nfrerebeau/tabula/branch/master)
[![Travis build
Status](https://travis-ci.org/nfrerebeau/tabula.svg?branch=master)](https://travis-ci.org/nfrerebeau/tabula)
[![codecov](https://codecov.io/gh/nfrerebeau/tabula/branch/master/graph/badge.svg)](https://codecov.io/gh/nfrerebeau/tabula)

[![CRAN
Version](http://www.r-pkg.org/badges/version/tabula)](https://cran.r-project.org/package=tabula)
[![CRAN
checks](https://cranchecks.info/badges/worst/tabula)](https://cran.r-project.org/web/checks/check_results_tabula.html)
[![CRAN
Downloads](http://cranlogs.r-pkg.org/badges/tabula)](https://cran.r-project.org/package=tabula)

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)

[![DOI
Zenodo](https://zenodo.org/badge/DOI/10.5281/zenodo.1489944.svg)](https://doi.org/10.5281/zenodo.1489944)
[![DOI
JOSS](https://joss.theoj.org/papers/10.21105/joss.01821/status.svg)](https://doi.org/10.21105/joss.01821)

## Overview

An easy way to examine archaeological count data. This package provides
a convenient and reproducible toolkit for relative and absolute dating
and analysis of (chronological) patterns. It includes functions for
matrix seriation (reciprocal ranking, CA-based seriation), chronological
modeling and dating of archaeological assemblages and/or objects. Beyond
these, the package provides several tests and measures of diversity:
heterogeneity and evenness (Brillouin, Shannon, Simpson, etc.), richness
and rarefaction (Chao1, Chao2, ACE, ICE, etc.), turnover and similarity
(Brainerd-Robinson, etc.). The package make it easy to visualize count
data and statistical thresholds: rank vs. abundance plots, heatmaps,
Ford (1962) and Bertin (1977) diagrams.

To cite **tabula** in publications please use:

> Frerebeau, N. (2019). tabula: An R Package for Analysis, Seriation,
> and Visualization of Archaeological Count Data. *Journal of Open
> Source Software*, 4(44), 1821. DOI
> [10.21105/joss.01821](https://doi.org/10.21105/joss.01821).

## Installation

You can install the released version of **tabula** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tabula")
```

Or install the development version from GitHub with:

``` r
# install.packages("devtools")
remotes::install_github("nfrerebeau/tabula")
```

## Usage

``` r
# Load packages
library(tabula)
#> Loading required package: codex

library(khroma)
library(ggplot2)
library(magrittr)
```

**tabula** uses a set of S4 classes that extend the basic `matrix` data
type. These new classes represent different special types of matrix:

  - Numeric matrix:
      - `CountMatrix` represents absolute frequency data,
      - `AbundanceMatrix` represents relative frequency data,
      - `OccurrenceMatrix` represents a co-occurrence matrix,
      - `SimilarityMatrix` represents a (dis)similarity matrix,
  - Logical matrix:
      - `IncidenceMatrix` represents presence/absence data,
      - `StratigraphicMatrix` represents stratigraphic relationships.

*It assumes that you keep your data tidy*: each variable (type/taxa)
must be saved in its own column and each observation (sample/case) must
be saved in its own row.

These new classes are of simple use, please refer to the documentation
of the [**codex**](https://github.com/nfrerebeau/codex) package where
these classes are defined.

### Visualization

Several types of graphs are available in **tabula** which uses
[**ggplot2**](https://github.com/tidyverse/ggplot2) for plotting
informations. This makes it easy to customize diagrams (e.g. using
themes and scales).

Spot matrix\[1\] allows direct examination of data:

``` r
# Plot co-occurrence of types
# (i.e. how many times (percent) each pairs of taxa occur together 
# in at least one sample.)
mississippi %>%
  as_occurrence() %>%
  plot_spot() +
  ggplot2::labs(size = "", colour = "Co-occurrence") +
  ggplot2::theme(legend.box = "horizontal") +
  khroma::scale_colour_YlOrBr()
```

<img src="man/figures/README-plot-occ-1.png" style="display: block; margin: auto;" />

Bertin or Ford (battleship curve) diagrams can be plotted, with
statistic threshold (including B. Desachy’s
[sériographe](https://doi.org/10.3406/pica.2004.2396)).

``` r
mississippi %>%
  as_count() %>%
  plot_bertin(threshold = mean) +
  khroma::scale_fill_vibrant()
```

<img src="man/figures/README-bertin-1.png" style="display: block; margin: auto;" />

``` r
compiegne %>%
  as_count() %>%
  plot_ford()
```

<img src="man/figures/README-ford-1.png" style="display: block; margin: auto;" />

### Seriation

``` r
# Build an incidence matrix with random data
set.seed(12345)
incidence <- IncidenceMatrix(data = sample(0:1, 400, TRUE, c(0.6, 0.4)),
                             nrow = 20)

# Get seriation order on rows and columns
# Correspondance analysis-based seriation
(indices <- seriate_reciprocal(incidence, margin = c(1, 2)))
#> <PermutationOrder: 4bffe51c-75f2-4bc3-a2d0-7c005a75b349>
#> Permutation order for matrix seriation:
#> - Row order: 1 4 20 3 9 16 19 10 13 2 11 7 17 5 6 18 14 15 8 12...
#> - Column order: 1 16 9 4 8 14 3 20 13 2 6 18 7 17 5 11 19 12 15 10...
#> - Method: reciprocal

# Permute matrix rows and columns
incidence2 <- permute(incidence, indices)
```

``` r
# Plot matrix
plot_heatmap(incidence) + 
  ggplot2::labs(title = "Original matrix") +
  ggplot2::scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white"))
plot_heatmap(incidence2) + 
  ggplot2::labs(title = "Rearranged matrix") +
  ggplot2::scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white"))
```

![](man/figures/README-permute-incidence-1.png)![](man/figures/README-permute-incidence-2.png)

### Dating

This package provides an implementation of the chronological modeling
method developed by Bellanger and Husi
([2012](https://doi.org/10.1016/j.jas.2011.06.031)). This method is
slightly modified here and allows the construction of different
probability density curves of archaeological assemblage dates (*event*,
*activity* and *tempo*). Note that this implementation is experimental
(see `help(date_event)`).

``` r
# Coerce dataset to abundance (count) matrix
zuni_counts <- as_count(zuni)
# Assume that some assemblages are reliably dated (this is NOT a real example)
# The names of the vector entries must match the names of the assemblages
set_dates(zuni_counts) <- c(
  LZ0569 = 1097, LZ0279 = 1119, CS16 = 1328, LZ0066 = 1111,
  LZ0852 = 1216, LZ1209 = 1251, CS144 = 1262, LZ0563 = 1206,
  LZ0329 = 1076, LZ0005Q = 859, LZ0322 = 1109, LZ0067 = 863,
  LZ0578 = 1180, LZ0227 = 1104, LZ0610 = 1074
)

# Model the event date for each assemblage
model <- date_event(zuni_counts, cutoff = 90)
# Plot activity and tempo distributions
plot_date(model, type = "activity", select = "LZ1105") +
  ggplot2::labs(title = "Activity plot") +
  ggplot2::theme_bw()
plot_date(model, type = "tempo", select = "LZ1105") +
  ggplot2::labs(title = "Tempo plot") +
  ggplot2::theme_bw()
```

![](man/figures/README-date-1.png)![](man/figures/README-date-2.png)

### Analysis

*Diversity* can be measured according to several indices (sometimes
referred to as indices of *heterogeneity*):

``` r
mississippi %>%
  as_count() %>%
  index_heterogeneity(method = "shannon")
#> <HeterogeneityIndex: 05b1f084-1042-4314-b597-72a5b9bb6d79>
#> - Method: shannon
#>             size     index
#> 10-P-1       153 1.2027955
#> 11-N-9       758 0.7646565
#> 11-N-1      1303 0.9293974
#> 11-O-10      638 0.8228576
#> 11-N-4      1266 0.7901428
#> 13-N-5        79 0.9998430
#> 13-N-4       241 1.2051989
#> 13-N-16      171 1.1776226
#> 13-O-11      128 1.1533432
#> 13-O-10      226 1.2884172
#> 13-P-1       360 1.1725355
#> 13-P-8       192 1.5296294
#> 13-P-10       91 1.7952443
#> 13-O-7      1233 1.1627477
#> 13-O-5      1709 1.0718463
#> 13-N-21      614 0.9205717
#> 12-O-5       424 1.1751002
#> Holden Lake  360 0.7307620
#> 13-N-15     1300 1.1270126
#> 12-N-3       983 1.0270291

## Test difference in Shannon diversity between assemblages
## (returns a matrix of adjusted p values)
mississippi[1:5, ] %>%
  as_count() %>%
  test_diversity()
#>               10-P-1       11-N-9       11-N-1   11-O-10
#> 11-N-9  0.000000e+00           NA           NA        NA
#> 11-N-1  3.609626e-08 8.538298e-05           NA        NA
#> 11-O-10 2.415845e-13 4.735511e-01 2.860461e-02        NA
#> 11-N-4  0.000000e+00 7.116363e-01 7.961107e-05 0.7116363
```

Note that `berger`, `mcintosh` and `simpson` methods return a
*dominance* index, not the reciprocal form usually adopted, so that an
increase in the value of the index accompanies a decrease in diversity.

Corresponding *evenness* (i.e. a measure of how evenly individuals are
distributed across the sample) can also be computed, as well as
*richness* and *rarefaction*.

Several methods can be used to ascertain the degree of *turnover* in
taxa composition along a gradient on qualitative (presence/absence)
data. It assumes that the order of the matrix rows (from *1* to *n*)
follows the progression along the gradient/transect.

Diversity can also be measured by addressing *similarity* between pairs
of sites:

``` r
## Calculate the Brainerd-Robinson index
## Plot the similarity matrix
mississippi %>%
  as_count() %>%
  similarity(method = "brainerd") %>%
  plot_spot() +
  ggplot2::labs(size = "Similarity", colour = "Similarity") +
  khroma::scale_colour_iridescent()
```

<img src="man/figures/README-similarity-brainerd-1.png" style="display: block; margin: auto;" />

The Frequency Increment Test can be used to assess the detection and
quantification of selective processes in the archaeological
record\[2\].

``` r
## Keep only decoration types that have a maximum frequency of at least 50
keep <- apply(X = merzbach, MARGIN = 2, FUN = function(x) max(x) >= 50)
merzbach_count <- as_count(merzbach[, keep])

## The data are grouped by phase
## We use the row names as time coordinates (roman numerals)
set_dates(merzbach_count) <- rownames(merzbach)
## Plot time vs abundance and highlight selection
plot_time(merzbach_count, highlight = "FIT", roll = TRUE) +
  ggplot2::theme_bw() +
  khroma::scale_color_contrast()
```

<img src="man/figures/README-plot-time-1.png" style="display: block; margin: auto;" />

## Contributing

Please note that the **tabula** project is released with a [Contributor
Code of
Conduct](https://github.com/nfrerebeau/tabula/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

1.  Adapted from Dan Gopstein’s original
    [idea](https://dgopstein.github.io/articles/spot-matrix/).

2.  Adapted from Ben Marwick’s original
    [idea](https://github.com/benmarwick/signatselect/).
