## Plot spot diagram of a co-occurrence matrix
data("mississippi", package = "folio")
mississippi <- as_count(mississippi)

occ <- as_occurrence(mississippi)
plot_spot(occ, diag = FALSE, upper = TRUE)
