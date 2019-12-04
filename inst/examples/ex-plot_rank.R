## Abundance matrix
## Coerce datasets to a count matrix (data from Desachy 2004)
compiegne_count <- codex::as_count(compiegne)

## Plot rank vs abundance
plot_rank(compiegne_count)
plot_rank(compiegne_count, facet = FALSE)
