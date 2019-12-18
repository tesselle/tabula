\donttest{
## Abundance data
## Coerce dataset to a count matrix
mississippi_count <- as_count(mississippi)

## Plot a Bertin diagram...
## ...without threshold
plot_bertin(mississippi_count, threshold = NULL)
## ...with the variable mean as threshold
plot_bertin(mississippi_count, threshold = mean)

## ...with variables scaled to 0-1
scale_01 <- function(x) (x - min(x)) / (max(x) - min(x))
plot_bertin(mississippi_count, threshold = mean, scale = scale_01)

## Abundance data
## Coerce dataset to a count matrix (data from Desachy 2004)
compiegne_count <- as_count(compiegne)

## Plot a Ford diagram...
## ...without threshold
plot_ford(compiegne_count)
## ...with EPPM
plot_ford(compiegne_count, EPPM = TRUE)
}
