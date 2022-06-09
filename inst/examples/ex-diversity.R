data("chevelon", package = "folio")

## Shannon diversity index
(h <- heterogeneity(chevelon, method = "shannon"))
(e <- evenness(chevelon, method = "shannon"))

## Bootstrap resampling (summary statistics)
bootstrap(h, f = summary)

## Jackknife resampling
jackknife(h)
