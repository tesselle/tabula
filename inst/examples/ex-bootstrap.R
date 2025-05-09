## Data from Conkey 1980, Kintigh 1989
data("cantabria")

## Shannon diversity index
(h <- heterogeneity(cantabria, method = "shannon"))

## Bootstrap resampling
bootstrap(h)

bootstrap(h, f = summary)

quant <- function(x) quantile(x, probs = c(0.05, 0.95))
bootstrap(h, f = quant)
