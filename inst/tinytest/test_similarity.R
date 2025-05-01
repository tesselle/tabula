# Plot Similarity ==============================================================
if (at_home()) {
  using("tinysnapshot")
  source("helpers.R")

  data("pueblo")
  test_sim <- similarity(pueblo, method = "brainerd")

  plot_mtx_sim <- function() plot_heatmap(test_sim, upper = TRUE)
  expect_snapshot_plot(plot_mtx_sim, "plot_mtx_sim")

  plot_spot_sim_ring <- function() plot_spot(test_sim, type = "ring")
  expect_snapshot_plot(plot_spot_sim_ring, "plot_spot_sim_ring")

  plot_spot_sim_plain <- function() plot_spot(test_sim, type = "plain")
  expect_snapshot_plot(plot_spot_sim_plain, "plot_spot_sim_plain")
}
