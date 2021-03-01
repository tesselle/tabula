\donttest{
## Coerce data to a count matrix
data("chevelon", package = "folio")
chevelon <- as_count(chevelon)

## Assemblage diversity size comparison
## Warning: this may take a few seconds!
sim_evenness <- simulate_evenness(chevelon, method = "shannon")
plot(sim_evenness)

sim_richness <- simulate_richness(chevelon, method = "none")
plot(sim_richness)
}
