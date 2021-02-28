\donttest{
## Abundance data
## Coerce dataset to a count matrix
data("mississippi", package = "folio")
counts1 <- as_count(mississippi)

## Plot a Bertin diagram...
## ...without threshold
plot_bertin(counts1)
## ...with variables scaled to 0-1 and the variable mean as threshold
scale_01 <- function(x) (x - min(x)) / (max(x) - min(x))
plot_bertin(counts1, threshold = mean, scale = scale_01)

## Abundance data
## Coerce dataset to a count matrix (data from Desachy 2004)
data("compiegne", package = "folio")
counts2 <- as_count(compiegne)

## Plot a Ford diagram...
## ...without threshold
plot_ford(counts2)
## ...with EPPM
plot_ford(counts2, EPPM = TRUE)
}
