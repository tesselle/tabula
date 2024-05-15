## Data from Conkey 1980, Kintigh 1989
data("cantabria")

## Shannon diversity index
(h <- heterogeneity(cantabria, method = "shannon"))

## Jackknife resampling
jackknife(h)

jackknife(h, f = summary)
