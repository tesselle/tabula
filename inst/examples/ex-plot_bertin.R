## Data from Lipo et al. 2015
data("mississippi", package = "folio")

## Plot a Bertin diagram...
## ...without threshold
plot_bertin(mississippi)

## ...with the variable mean as threshold
plot_bertin(mississippi, threshold = mean)

## ...with each variable scaled to 0-1 and the variable mean as threshold
scale_01 <- function(x) (x - min(x)) / (max(x) - min(x))
plot_bertin(mississippi, threshold = mean, scale = scale_01)
