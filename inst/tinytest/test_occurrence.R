if (at_home()) {
  source("helpers.R")
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  data("cantabria")
  test_occ <- occurrence(cantabria)

  plot_mtx_occ <- function() plot_heatmap(test_occ, upper = TRUE)
  expect_snapshot_plot(plot_mtx_occ, "plot_mtx_occ")

  plot_spot_occ <- function() plot_spot(test_occ, upper = FALSE)
  expect_snapshot_plot(plot_spot_occ, "plot_spot_occ")
}
