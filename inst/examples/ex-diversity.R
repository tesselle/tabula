## Coerce dataset to a count matrix
data("chevelon", package = "folio")
chevelon <- as_count(chevelon)

## Shannon diversity index
(index_h <- index_heterogeneity(chevelon, method = "shannon"))
(index_e <- index_evenness(chevelon, method = "shannon"))

## Bootstrap resampling
(boot_h <- bootstrap_heterogeneity(chevelon, method = "shannon"))

## Jackknife resampling
(jack_h <- jackknife_heterogeneity(chevelon, method = "shannon"))
