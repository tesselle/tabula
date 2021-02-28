library(magrittr)

\donttest{
## Assemblage diversity size comparison
## Warning: this may take a few seconds!
data("chevelon", package = "folio")

chevelon %>%
  as_count() %>%
  index_evenness(method = "shannon") %>%
  simulate() %>%
  plot()

chevelon %>%
  as_count() %>%
  index_richness(method = "none") %>%
  simulate() %>%
  plot()
}
