Sys.setenv(LANGUAGE = "en") # Force locale

data("cantabria")

# Jackknife ====================================================================
div <- heterogeneity(cantabria, method = "shannon")
jack <- jackknife(div)
expect_equal_to_reference(jack, file = "_snaps/heterogeneity_jackknife.rds")

eve <- evenness(cantabria, method = "shannon")
jack <- jackknife(eve)
expect_equal_to_reference(jack, file = "_snaps/evenness_jackknife.rds")

rich <- richness(cantabria, method = "observed")
jack <- jackknife(rich)
expect_equal_to_reference(jack, file = "_snaps/richness_jackknife.rds")

if (at_home() && Sys.info()["sysname"] != "Darwin") {
  using("tinysnapshot")
  source("helpers.R")

  # Bootstrap ==================================================================
  boot <- suppressWarnings(bootstrap(div, n = 30, seed = 12345))
  expect_true(all(boot$bias < 0)) # Downward bias
  expect_equal_to_reference(boot, file = "_snaps/heterogeneity_bootstrap.rds")

  boot <- suppressWarnings(bootstrap(eve, n = 30, seed = 12345))
  expect_equal_to_reference(boot, file = "_snaps/evenness_bootstrap.rds")

  boot <- suppressWarnings(bootstrap(rich, n = 30, seed = 12345))
  expect_true(all(boot$bias < 0)) # Downward bias
  expect_equal_to_reference(boot, file = "_snaps/richness_bootstrap.rds")

  # Simulate ===================================================================
  sim_diveristy <- simulate(div, n = 10, seed = 12345)
  plot_heterogeneity <- function() plot(sim_diveristy)
  expect_snapshot_plot(plot_heterogeneity, "plot_heterogeneity")

  sim_richness <- simulate(rich, n = 10, seed = 12345)
  plot_richness <- function() plot(sim_richness)
  expect_snapshot_plot(plot_richness, "plot_richness")
}
