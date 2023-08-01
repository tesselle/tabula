source("helpers.R")

# Heterogeneity ================================================================
data("cantabria")

method <- c("berger", "brillouin", "mcintosh", "simpson", "shannon")
for (i in method) {
  index <- heterogeneity(cantabria, method = i)
  expect_length(index, nrow(cantabria))
  expect_equal(get_method(index), i)
}

boot <- with_seed(12345, bootstrap(index, n = 30))
expect_equal_to_reference(boot, file = "_snaps/heterogeneity_bootstrap.rds")

jack <- jackknife(index)
expect_equal_to_reference(jack, file = "_snaps/heterogeneity_jackknife.rds")

# Evenness =====================================================================
data("cantabria")

method <- c("brillouin", "mcintosh", "simpson", "shannon")
for (i in method) {
  index <- evenness(cantabria, method = i)
  expect_length(index, nrow(cantabria))
  expect_equal(get_method(index), i)
}

boot <- with_seed(12345, bootstrap(index, n = 30))
expect_equal_to_reference(boot, file = "_snaps/evenness_bootstrap.rds")

jack <- jackknife(index)
expect_equal_to_reference(jack, file = "_snaps/evenness_jackknife.rds")

# Test =========================================================================
# Data from Magurran 1988, p. 145-149
birds <- matrix(
  data = c(35, 26, 25, 21, 16, 11, 6, 5, 3, 3,
           3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 0, 0,
           30, 30, 3, 65, 20, 11, 0, 4, 2, 14,
           0, 3, 9, 0, 0, 5, 0, 0, 0, 0, 1, 1),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(c("oakwood", "spruce"), NULL)
)
expect_equal_to_reference(test_diversity(birds), file = "_snaps/shannon_test.rds")

# Plot =========================================================================
# data("cantabria")
#
# skip_if_not_installed("vdiffr")
# idx_heterogeneity <- with_seed(12345, {
#   idx_heterogeneity <- heterogeneity(cantabria, method = "shannon")
#   sim_heterogeneity <- simulate(idx_heterogeneity, n = 100)
# })
# gg_heterogeneity <- autoplot(sim_heterogeneity)
# vdiffr::expect_doppelganger("idx_heterogeneity", gg_heterogeneity)
