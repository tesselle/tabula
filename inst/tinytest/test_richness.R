source("helpers.R")
data("cantabria")

# Richness =====================================================================
method <- c("margalef", "menhinick", "observed")
for (i in method) {
  index <- richness(cantabria, method = i)
  expect_length(index, nrow(cantabria))
  expect_equal(get_method(index), i)
}

boot <- with_seed(12345, bootstrap(index, n = 30))
expect_equal_to_reference(boot, file = "_snaps/richness_bootstrap.rds")

jack <- jackknife(index)
expect_equal_to_reference(jack, file = "_snaps/richness_jackknife.rds")

# Composition ==================================================================
# Data from Magurran 1988, p. 128-129
trap <- matrix(data = c(9, 3, 0, 4, 2, 1, 1, 0, 1, 0, 1, 1,
                        1, 0, 1, 0, 0, 0, 1, 2, 0, 5, 3, 0),
               nrow = 2, byrow = TRUE)

method <- c("chao2", "ice")
for (i in method) {
  index <- composition(trap, method = i)
  expect_length(index, 1)
  expect_equal(get_method(index), i)
}

# Plot =========================================================================
if (at_home()) {
  source("helpers.R")
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  idx_richness <- with_seed(12345, {
    idx_richness <- richness(cantabria, method = "observed")
    sim_richness <- simulate(idx_richness, n = 10)
  })
  plot_richness <- function() plot(sim_richness)
  expect_snapshot_plot(plot_richness, "plot_richness")
}
