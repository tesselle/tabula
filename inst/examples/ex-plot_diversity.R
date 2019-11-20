library(magrittr)

merzbach %>%
  as_count() %>%
  index_evenness(method = "shannon", simulate = FALSE) %>%
  plot_diversity()

\donttest{
## Assemblage diversity size comparison
## Warning: this may take a few seconds!
merzbach %>%
  as_count() %>%
  index_evenness(method = "shannon", simulate = TRUE) %>%
  plot_diversity()

merzbach %>%
  as_count() %>%
  index_richness(method = "none", simulate = TRUE) %>%
  plot_diversity()
}
