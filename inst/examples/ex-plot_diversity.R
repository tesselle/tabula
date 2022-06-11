\donttest{
## Coerce data to a count matrix
data("chevelon", package = "folio")

## Assemblage diversity size comparison
## Warning: this may take a few seconds!
h <- heterogeneity(chevelon, method = "shannon")
h_sim <- simulate(h)
plot(h_sim)

r <- richness(chevelon, method = "count")
r_sim <- simulate(r)
plot(r_sim)
}
