## Data from Conkey 1980, Kintigh 1989
data("cantabria")

## Plot spot diagram of a co-occurrence matrix
occ_abs <- occurrence(cantabria, method = "absolute") # Absolute frequencies
plot_spot(occ_abs)

occ_rel <- occurrence(cantabria, method = "relative") # Relative frequencies
plot_spot(occ_rel)

## Binomial co-occurrence (similarity between types)
occ_bin <- occurrence(cantabria, method = "binomial")
plot_spot(occ_bin)
