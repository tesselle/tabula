data("pueblo")
exp_occ <- matrix(
  data = c(0L, 5L, 5L, 1L, 1L,
           5L, 0L, 7L, 2L, 1L,
           5L, 7L, 0L, 2L, 3L,
           1L, 2L, 2L, 0L, 0L,
           1L, 1L, 3L, 0L, 0L),
  nrow = 5, ncol = 5
)
test_occ <- occurrence(pueblo, method = "absolute")
expect_equivalent(test_occ, as.dist(exp_occ))

## Binomial co-occurrence
x <- c(16, 9, 3, 0, 1)
y <- c(13, 3, 2, 0, 0)
expect_equal(round(index_binomial(x, y), 3), 0.537) # 0.54

if (at_home()) {
  using("tinysnapshot")
  source("helpers.R")

  plot_mtx_occ <- function() plot_heatmap(test_occ, upper = TRUE)
  expect_snapshot_plot(plot_mtx_occ, "plot_mtx_occ")

  plot_spot_occ <- function() plot_spot(test_occ, upper = FALSE)
  expect_snapshot_plot(plot_spot_occ, "plot_spot_occ")
}
