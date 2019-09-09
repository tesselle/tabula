## Abundance matrix
## Coerce datasets to a count matrix (data from Desachy 2004)
count <- as(compiegne, "CountMatrix")

## Plot rank vs abundance
plot_rank(count)
plot_rank(count, facet = FALSE)
