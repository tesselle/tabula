## Data from Conkey 1980, Kintigh 1989
data("cantabria")

## Alpha diversity
diversity(cantabria)

## Shannon diversity index
(h <- heterogeneity(cantabria, method = "shannon"))
(e <- evenness(cantabria, method = "shannon"))

as.data.frame(h)
