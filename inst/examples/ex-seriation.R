\dontrun{

# Refine matrix seriation (this is a long running example)
# Reproduces Peeples and Schachner 2012 results
count <- as(zuni, "CountMatrix")

## Samples with convex hull maximum dimension length greater than the cutoff
## value will be marked for removal.
## Define cutoff as one standard deviation above the mean
fun <- function(x) { mean(x) + sd(x) }

## Get indices of samples to be kept
## Warning: this may take a few seconds!
refined <- refine(count, cutoff = fun)
refined[["keep"]]

# Matrix seriation
# Reproduces Desachy 2004 results
## Coerce dataset to abundance matrix
count <- as(compiegne, "CountMatrix")

## Plot new matrix
plotBar(count, EPPM = TRUE)

## Get seriation order for columns on EPPM using the reciprocal averaging method
## Expected column order: N, A, C, K, P, L, B, E, I, M, D, G, O, J, F, H
indices <- seriate(count, method = "reciprocal", EPPM = TRUE, margin = 2)

## Permute columns
new <- permute(count, indices)

## Plot new matrix
plotBar(new, EPPM = TRUE)
}
