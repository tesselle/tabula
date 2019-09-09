## Coerce the merzbach dataset to a count matrix
## Keep only decoration types that have a maximum frequency of at least 50
data("merzbach")
keep <- apply(X = merzbach, MARGIN = 2, FUN = function(x) max(x) >= 50)
count_merzbach <- as(merzbach[, keep], "CountMatrix")

## The data are grouped by phase
## We use the row names as time coordinates (roman numerals)
set_dates(count_merzbach) <- rownames(merzbach)
## Plot time vs abundance
plot_time(count_merzbach)
## Plot time vs abundance and highlight selection
plot_time(count_merzbach, highlight = "FIT", roll = TRUE)