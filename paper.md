---
title: 'tabula: An R Package for Analysis, Seriation, and Visualization of Archaeological Count Data'
tags:
  - archaeology
  - matrix seriation
  - chronology
  - diversity measurement
authors:
  - name: Nicolas Frerebeau
    orcid: 0000-0001-5759-4944
    affiliation: "1, 2"
affiliations:
 - name: IRAMAT-CRP2A (UMR 5060 - CNRS, Université Bordeaux Montaigne)
   index: 1
 - name: Laboratoire de Mathématiques Jean Leray (UMR 6629 - CNRS, Université de Nantes)
   index: 2
date: 23 September 2019
bibliography: paper.bib
---

# Background

Detecting and quantifying material and cultural variations in time and space are important methodological issues in archaeology. To solve these issues, we need to construct reliable chronologies and quantitative descriptions of archaeological assemblages, i. e. archaeological sites or intrasite units, each described as a set of $p$ different objects.

Building chronologies involves distinguishing between relative (providing only a chronological sequence) and absolute dating methods (that yield calendric indicators) [@obrien2002]. Within relative dating, matrix seriation is a long-established method---it was first formulated by @petrie1899---and has allowed the construction of reference chronologies [@ihm2005]. For a set $X$ of $n$ archaeological assemblages, the seriation problem comes down to discovering in $X$ an order inferred as chronological. This approach relies on a set of well-defined statistical and archaeological assumptions [@dunnell1970]. It uses *a priori* information, e.g., absolute dates or stratigraphical constraints: @poblome2003, and allows the analysis of chronological patterns in a socio-economic or cultural perspective, e.g., @bellanger2012, @lipo2015.

The quantitative analysis of archaeological assemblages can thus be carried out in a synchronic, e.g., diversity measurements, or diachronic, e.g., evolutionary studies: selection process, patterns of cultural transmission, etc., way. These approaches cover a wide range of applications and have led to the development of a multitude of statistical models, but none have been systematically implemented to enable the deployment of reproducible workflows.

# Summary

`tabula` provides a convenient and reproducible toolkit for analyzing, seriating, and visualizing archaeological count data, e.g., artifacts, faunal remains, etc.

The package uses a set of S4 classes for archaeological data matrices that extend the `matrix` data type. These new classes represent different specialized matrices: incidence, abundance, co-occurrence, and (dis)similarity. Methods for a variety of functions applied to objects from these classes provide tools for relative and absolute dating and analysis of (chronological) patterns.

`tabula` includes functions for matrix seriation (`seriate_*`), as well as chronological modeling and dating (`date_*`) of archaeological assemblages and objects. Resulting models can be checked for stability and refined with resampling methods (`refine_*`). Estimated dates can then be displayed as tempo or activity plot [@dye2016] to assess rhythms over long periods. Beyond these, `tabula` provides several tests (`test_*`) and measures of diversity within and between archaeological assemblages: heterogeneity and evenness (Brillouin, Shannon, Simpson, etc.), richness and rarefaction (Chao1, Chao2, ACE, ICE, etc.), turnover and similarity (Brainerd-Robinson, etc.). Finally, the package makes it easy to visualize count data and statistical thresholds (`plot_*`): rank vs. abundance plots, heatmaps, @ford1962, and @bertin1977 diagrams.

`tabula` is designed to be used both by archaeologists and by students in courses on dating methods and applied statistics in archaeology.

# Acknowledgements
All the contributors have made it possible to develop this project through their helpful discussion and by bringing in new ideas: Jean-Baptiste Fourvel, Brice Lebrun, Ben Marwick, Matthew Peeples, and Anne Philippe.

# References
