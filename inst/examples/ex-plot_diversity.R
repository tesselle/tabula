library(magrittr)

\donttest{
## Assemblage diversity size comparison
## Warning: this may take a few seconds!
altamira %>%
  as_count() %>%
  index_evenness(method = "shannon") %>%
  simulate() %>%
  plot()

altamira %>%
  as_count() %>%
  index_richness(method = "none") %>%
  simulate() %>%
  plot()
}
