## Data from Conkey 1980, Kintigh 1989
data("cantabria")

## Shannon diversity index
(h <- heterogeneity(cantabria, method = "shannon"))

## Bootstrap resampling
bootstrap(h, f = NULL)

bootstrap(h, f = summary)

quant <- function(x) quantile(x, probs = c(0.25, 0.50))
bootstrap(h, f = quant)

## Jackknife resampling
jackknife(h)

bootstrap(h, f = summary)
