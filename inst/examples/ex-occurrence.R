## Ceramic data
data("mississippi", package = "folio")

## Plot spot diagram of a co-occurrence matrix
occ <- occurrence(mississippi)
plot_spot(occ)
