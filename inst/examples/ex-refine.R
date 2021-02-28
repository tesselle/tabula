## Data from Magurran 1988, p. 145-149
data("birds", package = "folio")
birds <- as_count(birds)

## Shannon diversity
h <- index_heterogeneity(birds, "shannon")
bootstrap(h)
jackknife(h)
