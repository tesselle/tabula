## Abundance matrix
## Coerce datasets to a count matrix (data from Desachy 2004)
data("compiegne", package = "folio")

## Plot rank vs abundance
plot_rank(compiegne)
plot_rank(compiegne, facet = TRUE)
