## Coerce the merzbach dataset to a count matrix
## Keep only decoration types that have a maximum frequency of at least 50
keep <- apply(X = merzbach, MARGIN = 2, FUN = function(x) max(x) >= 50)
merzbach_count <- as_count(merzbach[, keep])

## The data are grouped by phase
## We use the row names as time coordinates (roman numerals)
set_dates(merzbach_count) <- rownames(merzbach)
## Plot time vs abundance
plot_time(merzbach_count)
## Plot time vs abundance and highlight selection
plot_time(merzbach_count, highlight = "FIT", roll = TRUE)
