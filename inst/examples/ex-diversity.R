data("chevelon", package = "folio")

## Shannon diversity index
(h <- heterogeneity(chevelon, method = "shannon"))
(e <- evenness(chevelon, method = "shannon"))

## Bootstrap resampling (summary statistics)
bootstrap(h, f = NULL)

bootstrap(h, f = summary)

quant <- function(x) quantile(x, probs = c(0.25, 0.50))
bootstrap(h, f = quant)

## Jackknife resampling
jackknife(h)
