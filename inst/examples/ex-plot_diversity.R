\donttest{
## Coerce data to a count matrix
data("chevelon", package = "folio")
chevelon <- as_count(chevelon)

## Assemblage diversity size comparison
## Warning: this may take a few seconds!
idx_heterogeneity <- index_heterogeneity(chevelon, method = "shannon")
sim_heterogeneity <- simulate(idx_heterogeneity)
plot(sim_heterogeneity)

idx_richness <- index_richness(chevelon, method = "none")
sim_richness <- simulate(idx_richness)
plot(sim_richness)
}
