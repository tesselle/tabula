## Data from Conkey 1980, Kintigh 1989
data("cantabria")

## Shannon diversity index
(h <- heterogeneity(cantabria, method = "shannon"))
(e <- evenness(cantabria, method = "shannon"))

plot(h)
as.data.frame(h)
