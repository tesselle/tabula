## Abundance matrix
## Coerce datasets to a count matrix (data from Desachy 2004)
data("compiegne", package = "folio")
counts <- as_count(compiegne)

## Plot rank vs abundance
plot_rank(counts)
plot_rank(counts, facet = TRUE)
