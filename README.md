
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabula <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/tesselle/tabula/workflows/R-CMD-check/badge.svg)](https://github.com/tesselle/tabula/actions)
[![codecov](https://codecov.io/gh/tesselle/tabula/branch/master/graph/badge.svg)](https://app.codecov.io/gh/tesselle/tabula)
[![CodeFactor](https://www.codefactor.io/repository/github/tesselle/tabula/badge)](https://www.codefactor.io/repository/github/tesselle/tabula)
[![Dependencies](https://tinyverse.netlify.com/badge/tabula)](https://cran.r-project.org/package=tabula)

<a href="https://tesselle.r-universe.dev" class="pkgdown-devel"><img
src="https://tesselle.r-universe.dev/badges/tabula"
alt="r-universe" /></a>
<a href="https://cran.r-project.org/package=tabula"
class="pkgdown-release"><img
src="http://www.r-pkg.org/badges/version/tabula"
alt="CRAN Version" /></a> <a
href="https://cran.r-project.org/web/checks/check_results_tabula.html"
class="pkgdown-release"><img
src="https://cranchecks.info/badges/worst/tabula"
alt="CRAN checks" /></a>
<a href="https://cran.r-project.org/package=tabula"
class="pkgdown-release"><img
src="http://cranlogs.r-pkg.org/badges/tabula"
alt="CRAN Downloads" /></a>

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

[![DOI
Zenodo](https://zenodo.org/badge/DOI/10.5281/zenodo.1489944.svg)](https://doi.org/10.5281/zenodo.1489944)
[![DOI
JOSS](https://joss.theoj.org/papers/10.21105/joss.01821/status.svg)](https://doi.org/10.21105/joss.01821)
<!-- badges: end -->

## Overview

An easy way to examine archaeological count data. **tabula** provides
several tests and measures of diversity: heterogeneity and evenness
(Brillouin, Shannon, Simpson, etc.), richness and rarefaction (Chao1,
Chao2, ACE, ICE, etc.), turnover and similarity (Brainerd-Robinson,
etc.). The package make it easy to visualize count data and statistical
thresholds: rank vs. abundance plots, heatmaps, Ford (1962) and Bertin
(1977) diagrams.

[**kairos**](https://packages.tesselle.org/kairos/) is a companion
package to **tabula** that provides functions for chronological modeling
and dating of archaeological assemblages from count data.


    To cite tabula in publications use:

      Frerebeau, Nicolas (2019). tabula: An R Package for Analysis,
      Seriation, and Visualization of Archaeological Count Data. Journal of
      Open Source Software, 4(44), 1821. DOI 10.21105/joss.01821.

    Une entrée BibTeX pour les utilisateurs LaTeX est

      @Article{,
        title = {{tabula}: An R Package for Analysis, Seriation, and Visualization of Archaeological Count Data},
        author = {Nicolas Frerebeau},
        year = {2019},
        journal = {Journal of Open Source Software},
        volume = {4},
        number = {44},
        page = {1821},
        doi = {10.21105/joss.01821},
      }

## Installation

You can install the released version of **tabula** from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tabula")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("tesselle/tabula")
```

## Usage

``` r
## Load packages
library(folio) # Datasets
library(khroma) # Color scales
library(ggplot2)

library(tabula)
```

*It assumes that you keep your data tidy*: each variable (type/taxa)
must be saved in its own column and each observation (sample/case) must
be saved in its own row.

### Visualization

Several types of graphs are available in **tabula** which uses
[**ggplot2**](https://ggplot2.tidyverse.org) for plotting informations.
This makes it easy to customize diagrams (e.g. using themes and scales).

Bertin or Ford (battleship curve) diagrams can be plotted, with
statistic threshold (including B. Desachy’s
[sériographe](https://doi.org/10.3406/pica.2004.2396)).

``` r
## Bertin matrix with variables scaled to 0-1 and the variable mean as threshold
scale_01 <- function(x) (x - min(x)) / (max(x) - min(x))

plot_bertin(mississippi, threshold = mean, scale = scale_01) +
  khroma::scale_fill_vibrant(name = "Mean")
```

<img src="man/figures/README-bertin-1.png" style="display: block; margin: auto;" />

``` r
## Ford diagram
plot_ford(mississippi)
```

<img src="man/figures/README-ford-1.png" style="display: block; margin: auto;" />

Spot matrix[^1] allows direct examination of data:

``` r
## Plot co-occurrence of types
## (i.e. how many times (percent) each pairs of taxa occur together 
## in at least one sample.)
plot_spot(mississippi, freq = TRUE) +
  ggplot2::labs(size = "Co-occurrence", colour = "Co-occurrence") +
  khroma::scale_colour_YlOrBr()
```

<img src="man/figures/README-plot-occ-1.png" style="display: block; margin: auto;" />

### Diversity

*Diversity* can be measured according to several indices (referred to as
indices of *heterogeneity* – see `vignette("diversity")`). Corresponding
*evenness* (i.e. a measure of how evenly individuals are distributed
across the sample) can also be computed, as well as *richness* and
*rarefaction*.

``` r
heterogeneity(mississippi, method = "shannon")
#>  [1] 1.2027955 0.7646565 0.9293974 0.8228576 0.7901428 0.9998430 1.2051989
#>  [8] 1.1776226 1.1533432 1.2884172 1.1725355 1.5296294 1.7952443 1.1627477
#> [15] 1.0718463 0.9205717 1.1751002 0.7307620 1.1270126 1.0270291
```

Measure diversity by comparing to simulated assemblages:

``` r
set.seed(12345)

## Data from Conkey 1980, Kintigh 1989, p. 28
chevelon |>
  heterogeneity(method = "shannon") |>
  simulate() |>
  plot()

chevelon |>
  richness(method = "count") |>
  simulate() |>
  plot()
```

![](man/figures/README-sample-size-1.png)![](man/figures/README-sample-size-2.png)

Several methods can be used to ascertain the degree of *turnover* in
taxa composition along a gradient on qualitative (presence/absence)
data. It assumes that the order of the matrix rows (from *1* to *n*)
follows the progression along the gradient/transect.

Diversity can also be measured by addressing *similarity* between pairs
of sites:

``` r
## Calculate the Brainerd-Robinson index
## Plot the similarity matrix
s <- similarity(mississippi, method = "brainerd")

plot_spot(s) +
  khroma::scale_colour_iridescent(name = "brainerd")
```

<img src="man/figures/README-similarity-brainerd-1.png" style="display: block; margin: auto;" />

## Contributing

Please note that the **tabula** project is released with a [Contributor
Code of Conduct](https://www.tesselle.org/conduct.html). By contributing
to this project, you agree to abide by its terms.

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-bertin1977" class="csl-entry">

Bertin, Jacques. 1977. *La graphique et le traitement graphique de
l’information*. Nouvelle bibliothèque scientifique. Paris: Flammarion.

</div>

<div id="ref-ford1962" class="csl-entry">

Ford, J. A. 1962. *A Quantitative Method for Deriving Cultural
Chronology*. Technical Manual 1. Washington, DC: Pan American Union.

</div>

</div>

[^1]: Adapted from Dan Gopstein’s original
    [idea](https://dgopstein.github.io/articles/spot-matrix/).
