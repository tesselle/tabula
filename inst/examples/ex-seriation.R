## Replicates Desachy 2004 results
## Coerce dataset to abundance matrix
data("compiegne", package = "folio")
compiegne_count <- as_count(compiegne)

## Get seriation order for columns on EPPM using the reciprocal averaging method
## Expected column order: N, A, C, K, P, L, B, E, I, M, D, G, O, J, F, H
(compiegne_indices <- seriate_rank(compiegne_count, EPPM = TRUE, margin = 2))

## Permute columns
compiegne_new <- permute(compiegne_count, compiegne_indices)

## Plot new matrix
plot_ford(compiegne_new, EPPM = FALSE)

## See the vignette:
\dontrun{
utils::vignette("seriation")
}
