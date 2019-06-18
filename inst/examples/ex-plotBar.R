## Load the mississippi dataset
data("mississippi")
## Coerce the mississippi dataset to a count matrix
mississippi_count <- as(mississippi, "CountMatrix")

## Plot a Bertin diagram...
## ...without threshold
plotBertin(mississippi_count, threshold = NULL)
## ...with the variable mean as threshold
plotBertin(mississippi_count, threshold = mean)

## ...with variables scaled to 0-1
scale_01 <- function(x){
  (x - min(x)) / (max(x) - min(x))
}
plotBertin(mississippi_count, threshold = mean, scale = scale_01)

## Load the mississippi dataset
data("compiegne")
## Coerce the compiegne dataset to a count matrix
compiegne_count <- as(compiegne, "CountMatrix")

## Plot a Ford diagram...
## ...without threshold
plotFord(compiegne_count)
## ...with EPPM
plotFord(compiegne_count, EPPM = TRUE)
