\donttest{
## Abundance data
data("mississippi", package = "folio")

## Plot a Bertin diagram...
## ...without threshold
plot_bertin(mississippi)
## ...with variables scaled to 0-1 and the variable mean as threshold
scale_01 <- function(x) (x - min(x)) / (max(x) - min(x))
plot_bertin(mississippi, threshold = mean, scale = scale_01)

## Abundance data
## Coerce dataset to a count matrix (data from Desachy 2004)
data("compiegne", package = "folio")

## Plot a Ford diagram...
## ...without threshold
plot_ford(compiegne)
## ...with EPPM
plot_ford(compiegne, EPPM = TRUE)
}
