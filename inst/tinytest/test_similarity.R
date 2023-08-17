# Similiraty measure (count data) ==============================================
# data("aves")
# expect_equal(round(similarity(aves, method = "bray"), 3), 0.444)

# Plot Similarity ==============================================================
if (at_home()) {
  source("helpers.R")
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  data("pueblo")
  test_sim <- similarity(pueblo, method = "brainerd")

  plot_mtx_sim <- function() plot_heatmap(test_sim, upper = TRUE)
  expect_snapshot_plot(plot_mtx_sim, "plot_mtx_sim")

  plot_spot_sim_ring <- function() plot_spot(test_sim, type = "ring")
  expect_snapshot_plot(plot_spot_sim_ring, "plot_spot_sim_ring")

  plot_spot_sim_plain <- function() plot_spot(test_sim, type = "plain")
  expect_snapshot_plot(plot_spot_sim_plain, "plot_spot_sim_plain")
}
