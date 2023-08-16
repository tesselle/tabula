data("pueblo")
exp_occ <- matrix(
  data = c(0L, 5L, 5L, 1L, 1L,
           5L, 0L, 7L, 2L, 1L,
           5L, 7L, 0L, 2L, 3L,
           1L, 2L, 2L, 0L, 0L,
           1L, 1L, 3L, 0L, 0L),
  nrow = 5, ncol = 5
)
test_occ <- occurrence(pueblo)
expect_equivalent(test_occ, as.dist(exp_occ))

if (at_home()) {
  source("helpers.R")
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  plot_mtx_occ <- function() plot_heatmap(test_occ, upper = TRUE)
  expect_snapshot_plot(plot_mtx_occ, "plot_mtx_occ")

  plot_spot_occ <- function() plot_spot(test_occ, upper = FALSE)
  expect_snapshot_plot(plot_spot_occ, "plot_spot_occ")
}
