birds <- matrix(
  data = c(1.4, 4.3, 2.9, 8.6, 4.2, 15.7, 2.0, 50, 1, 11.4, 11.4, 4.3, 13.0,
           14.3, 8.6, 7.1, 10.0, 1.4, 2.9, 5.7, 1.4, 11.4, 2.9, 4.3, 1.4, 2.9,
           0, 0, 0, 2.9, 0, 0, 0, 10, 0, 0, 5.7, 2.5, 5.7, 8.6, 5.7, 2.9, 0, 0,
           2.9, 0, 0, 5.7, 0, 2.9, 0, 2.9),
  nrow = 2, byrow = TRUE
)
rownames(birds) <- c("unmanaged", "managed")

# Similiraty measure (count data) ==============================================
# method <- c("bray", "jaccard", "morisita", "sorenson")
# expect_equal(round(similarity(birds, method = "bray"), 3), 0.444)

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
