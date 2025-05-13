Sys.setenv(LANGUAGE = "en") # Force locale

source("helpers.R")
data("cantabria")

# Diversity ====================================================================
d <- diversity(cantabria)
expect_identical(dim(d), c(5L, 11L))

# Heterogeneity ================================================================
method <- c("berger", "brillouin", "mcintosh", "simpson", "shannon")
for (i in method) {
  index <- heterogeneity(cantabria, method = i)
  expect_length(index, nrow(cantabria))
  expect_equal(get_method(index), i)
}

boot <- suppressWarnings(bootstrap(index, n = 30, seed = 12345))
expect_true(all(boot$bias < 0)) # Downward bias
expect_equal_to_reference(boot, file = "_snaps/heterogeneity_bootstrap.rds")

jack <- jackknife(index)
expect_equal_to_reference(jack, file = "_snaps/heterogeneity_jackknife.rds")

# Evenness =====================================================================
method <- c("brillouin", "mcintosh", "simpson", "shannon")
for (i in method) {
  index <- evenness(cantabria, method = i)
  expect_length(index, nrow(cantabria))
  expect_equal(get_method(index), i)
}

boot <- suppressWarnings(bootstrap(index, n = 30, seed = 12345))
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

# Same results as PAST v4.16c
# index_shannon(birds[1, ]) # 2.408
# tabula:::variance_shannon(birds[1, ]) # 0.0054004
# index_shannon(birds[2, ]) # 2.054
# tabula:::variance_shannon(birds[2, ]) # 0.0045188

t_shannon <- test_shannon(birds[1, ], birds[2, ])
expect_equal(round(t_shannon$statistic, 4), 3.5342)
expect_equal(round(t_shannon$parameter, 2), 358.19)
expect_equal(round(t_shannon$p.value, 5), 0.00046)

# index_simpson(birds[1, ]) # 0.11993
# tabula:::variance_simpson(birds[1, ]) # 0.00010543
# index_simpson(birds[2, ]) # 0.1757
# tabula:::variance_simpson(birds[2, ]) # 0.0002717

t_simpson <- test_simpson(birds[1, ], birds[2, ])
expect_equal(round(t_simpson$statistic, 4), -2.8716)
expect_equal(round(t_simpson$parameter, 2), 324.56)
expect_equal(round(t_simpson$p.value, 5), 0.00435)

expect_equal_to_reference(test_shannon(birds),
                          file = "_snaps/shannon_test.rds")
expect_equal_to_reference(test_simpson(birds),
                          file = "_snaps/simpson_test.rds")

# Plot =========================================================================
if (at_home()) {
  using("tinysnapshot")
  source("helpers.R")

  idx_heterogeneity <- with_seed(12345, {
    idx_heterogeneity <- heterogeneity(cantabria, method = "shannon")
    sim_heterogeneity <- simulate(idx_heterogeneity, n = 10)
  })
  plot_heterogeneity <- function() plot(sim_heterogeneity)
  expect_snapshot_plot(plot_heterogeneity, "plot_heterogeneity")

  ## SHE analysis
  plot_she <- function() she(cantabria)
  expect_snapshot_plot(plot_she, "plot_she")

  ## Diversity profiles
  plot_profiles <- function() profiles(cantabria)
  expect_snapshot_plot(plot_profiles, "plot_profiles")
}
