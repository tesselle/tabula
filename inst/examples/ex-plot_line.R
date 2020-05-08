## Coerce the merzbach dataset to a count matrix
## Keep only decoration types that have a maximum frequency of at least 50
keep <- apply(X = merzbach, MARGIN = 2, FUN = function(x) max(x) >= 50)
counts <- as_count(merzbach[, keep])

## The data are grouped by phase
## We use the row names as time coordinates (roman numerals)
dates <- as.numeric(utils::as.roman(rownames(counts)))
## Plot time vs abundance
plot_time(counts, dates)
## Plot time vs abundance and highlight selection
plot_time(counts, dates, highlight = "FIT", roll = TRUE)
