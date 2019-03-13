# Matrix seriation
# Reproduces Desachy 2004 results
## Coerce dataset to abundance matrix
compiegne_count <- as(compiegne, "CountMatrix")

## Get seriation order for columns on EPPM using the reciprocal averaging method
## Expected column order: N, A, C, K, P, L, B, E, I, M, D, G, O, J, F, H
(compiegne_indices <- seriate(compiegne_count, method = "reciprocal",
                              EPPM = TRUE, margin = 2))

## Permute columns
compiegne_new <- permute(compiegne_count, compiegne_indices)

## Plot new matrix
plotBar(compiegne_new, EPPM = FALSE)

\donttest{
# Refine matrix seriation (this is a long running example)
# Reproduces Peeples and Schachner 2012 results
zuni_count <- as(zuni, "CountMatrix")

## Samples with convex hull maximum dimension length greater than the cutoff
## value will be marked for removal.
## Define cutoff as one standard deviation above the mean
fun <- function(x) { mean(x) + sd(x) }

## Get indices of samples to be kept
## Warning: this may take a few seconds!
set.seed(123)
(zuni_refined <- refine(zuni_count, cutoff = fun))

## Get CA-based seriation order
(zuni_indices <- seriate(zuni_count, zuni_refined, margin = 1))
}
