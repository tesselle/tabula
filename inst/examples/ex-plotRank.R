## Coerce datasets to abundance matrix
## Data from Desachy 2004
data("compiegne")
count_compiegne <- as(compiegne, "CountMatrix")

## Plot rank vs abundance
plotRank(count_compiegne)
plotRank(count_compiegne, facet = FALSE)
