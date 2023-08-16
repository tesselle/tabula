## Data from Lipo et al. 2015
data("mississippi", package = "folio")

## Plot a Bertin diagram...
## ...without threshold
plot_bertin(mississippi)

## ...with the variable mean as threshold
plot_bertin(mississippi, threshold = mean)

## Plot conditional proportions
plot_bertin(mississippi, freq = TRUE, margin = 1)
plot_bertin(mississippi, freq = TRUE, margin = 2)
