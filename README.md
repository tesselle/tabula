
<!-- README.md is generated from README.Rmd. Please edit that file -->
tabula <img src="man/figures/logo.png" align="right" />
=======================================================

[![Build Status](https://travis-ci.org/nfrerebeau/tabula.svg?branch=master)](https://travis-ci.org/nfrerebeau/tabula) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tabula)](https://cran.r-project.org/package=tabula) [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

Overview
--------

`tabula` provides an easy way to examine archaeological count data (artifacts, faunal remains, etc.). This package includes several measures of diversity, e.g. richness, rarefaction, diversity, evenness, turnover and similarity. It also provides some matrix seriation methods for chronological modeling. The package make it easy to visualize count data and statistical thresholds: rank/abundance plots, Ford and Bertin diagrams, etc.

Installation
------------

Install the released version of `tabula` from CRAN:

``` r
install.packages("tabula")
```

Or install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("nfrerebeau/tabula")
```

Usage
-----

`tabula` provides a set of S4 classes that extend the `matrix` data type from R `base`. These new classes represent different special types of matrix:

-   Numeric matrix:
    -   `CountMatrix` represents count data,
    -   `FrequencyMatrix` represents frequency data,
-   Logical matrix:
    -   `IncidenceMatrix` represents presence/absence data,
    -   `StratigraphicMatrix` represents a stratigraphic sequence.

It assumes that you keep your data tidy: each variable (taxa) must be saved in its own column and each observation (case) must be saved in its own row.

These new classes are of simple use, on the same way as the base `matrix`:

``` r
# Define a count data matrix
quanti <- CountMatrix(data = 1:100, nrow = 10, ncol = 10, byrow = TRUE)

# Define à logical matrix
# Data will be coerced with as.logical()
quali <- IncidenceMatrix(data = sample(0:1, 100, TRUE), nrow = 10, ncol = 10)
```

`tabula` uses coercing mechanisms (with validation methods) for data type conversions:

``` r
data("compiegne") # A dataset of ceramic counts

# Define as a count matrix
count <- as(compiegne, "CountMatrix")

# Transform counts to frequencies
freq <- as(compiegne, "FrequencyMatrix")

# Row sums is internally stored before coercion to a frequency matrix
# This allows to restore the source data
count <- as(freq, "CountMatrix")
```

### Analysis

#### Sample richness

``` r
richness(count, method = c("margalef", "menhinick", "berger"), simplify = TRUE)
#>   margalef  menhinick
#> 5 1.176699 0.07933617
#> 4 1.323459 0.07568907
#> 3 1.412383 0.07905694
#> 2 1.429741 0.08432155
#> 1 1.428106 0.08381675
```

#### Heterogeneity and evenness measures

*Diversity* can be measured according to several indices (sometimes refered to as indices of *heterogeneity*):

``` r
diversity(count, method = c("shannon", "brillouin", "simpson", "mcintosh", "berger"), simplify = TRUE)
#>    shannon brillouin   simpson  mcintosh    berger
#> 5 1.311123  1.309565 0.3648338 0.3983970 0.5117318
#> 4 1.838332  1.836827 0.2246218 0.5287042 0.3447486
#> 3 2.037649  2.036142 0.1718061 0.5883879 0.3049316
#> 2 2.468108  2.466236 0.1038536 0.6812886 0.1927510
#> 1 2.297495  2.295707 0.1267866 0.6472862 0.1893524
```

Note that `berger`, `mcintosh` and `simpson` methods return a *dominance* index, not the reciprocal form usually adopted, so that an increase in the value of the index accompanies a decrease in diversity.

*Evenness* is a measure of how evenly individuals are distributed across the sample:

``` r
evenness(count, method = c("shannon", "brillouin", "simpson", "mcintosh"), simplify = TRUE)
#>     shannon brillouin   simpson  mcintosh
#> 5 0.5111691 0.5109738 0.2108442 0.5479357
#> 4 0.6788396 0.6787091 0.2967952 0.7091340
#> 3 0.7349264 0.7348441 0.3637822 0.7806408
#> 2 0.8901817 0.8901334 0.6018087 0.9035975
#> 1 0.8286460 0.8285786 0.4929544 0.8585271
```

#### Turnover

The following method can be used to acertain the degree of *turnover* in taxa composition along a gradient (*β*-diversity) on qualitative (presence/absence) data.

It assumes that the order of the matrix rows (from 1 to *n*) follows the progression along the gradient/transect.

``` r
turnover(count, method = c("whittaker", "cody", "routledge1",
                           "routledge2", "routledge3", "wilson"),
         simplify = TRUE)
#>  whittaker       cody routledge1 routledge2 routledge3     wilson 
#> 0.05263158 1.50000000 0.00000000 0.04061480 1.04145086 0.09868421
```

#### Similarity coefficients

*β*-diversity can also be measured by addressing *similarity* between pairs of sites:

``` r
similarity(count, method = "morisita")
#>           5         4         3         2         1
#> 5 1.0000000 0.9162972 0.7575411 0.6286479 0.7106784
#> 4 0.9162972 1.0000000 0.6670201 0.8879556 0.8251501
#> 3 0.7575411 0.8879556 1.0000000 0.7964064 0.6637747
#> 2 0.6670201 0.7964064 0.8251501 1.0000000 0.9224228
#> 1 0.6286479 0.7106784 0.6637747 0.9224228 1.0000000
```

### Visualization

Ranks *vs* abundance plot can be used for abundance models (model fitting will be implemented in a futur release):

``` r
plotRank(count, log = "xy")
```

![](man/figures/README-rank-1.png)

Spot matrix (no doubt easier to read than a heatmap) allows direct examination of data (above/below some threshold):

``` r
plotSpot(count, threshold = "mean")
```

![](man/figures/README-spot-1.png)

Bertin of Ford diagramms can be plotted, with statistic threshold (B. Desachy's seriograph):

``` r
plotBar(count, EPPM = TRUE)
```

![](man/figures/README-seriograph-1.png)

Matrix plot can produces a visual *χ*<sup>2</sup> (B. Desachy's matrigraph):

``` r
plotMatrix(count, PVI = TRUE)
```

![](man/figures/README-matrigraph-1.png)
