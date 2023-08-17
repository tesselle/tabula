if (at_home()) {
  source("helpers.R")
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  data("cantabria")
  data("pueblo")

  # Bertin =====================================================================
  plot_bertin_count <- function() plot_bertin(pueblo)
  expect_snapshot_plot(plot_bertin_count, "plot_bertin_count")
  plot_bertin_noflip <- function() plot_bertin(pueblo, flip = FALSE)
  expect_snapshot_plot(plot_bertin_noflip, "plot_bertin_noflip")

  ## Threshold
  plot_bertin_threshold <- function() plot_bertin(pueblo, threshold = mean)
  expect_snapshot_plot(plot_bertin_threshold, "plot_bertin_threshold")

  ## Plot conditional proportions
  plot_bertin_prop1 <- function() plot_bertin(pueblo, freq = TRUE, margin = 1)
  expect_snapshot_plot(plot_bertin_prop1, "plot_bertin_prop1")
  plot_bertin_prop2 <- function() plot_bertin(pueblo, freq = TRUE, margin = 2)
  expect_snapshot_plot(plot_bertin_prop2, "plot_bertin_prop2")

  # Ford =======================================================================
  plot_ford_count <- function() plot_ford(pueblo)
  expect_snapshot_plot(plot_ford_count, "plot_ford_count")
  plot_ford_weights <- function() plot_ford(pueblo, weights = TRUE)
  expect_snapshot_plot(plot_ford_weights, "plot_ford_weights")

  # Seriograph =================================================================
  plot_seriograph <- function() seriograph(pueblo)
  expect_snapshot_plot(plot_seriograph, "plot_seriograph")

  # Dice-Leraas ================================================================
  plot_dice_leraas <- function() plot_diceleraas(pueblo)
  expect_snapshot_plot(plot_dice_leraas, "plot_dice_leraas")

  # Heatmap ====================================================================
  ## Count data
  plot_mtx_count <- function() plot_heatmap(cantabria)
  expect_snapshot_plot(plot_mtx_count, "plot_mtx_count")

  ## Conditional proportions
  plot_mtx_prop1 <- function() plot_heatmap(cantabria, freq = TRUE, margin = 1)
  expect_snapshot_plot(plot_mtx_prop1, "plot_mtx_prop1")
  plot_mtx_prop2 <- function() plot_heatmap(cantabria, freq = TRUE, margin = 2)
  expect_snapshot_plot(plot_mtx_prop2, "plot_mtx_prop2")

  ## Incidence data
  plot_mtx_incid <- function() plot_heatmap(cantabria > 0)
  expect_snapshot_plot(plot_mtx_incid, "plot_mtx_incid")

  # Matrigraph =================================================================
  plot_matrigraph <- function() matrigraph(cantabria, reverse = FALSE)
  expect_snapshot_plot(plot_matrigraph, "plot_matrigraph")

  plot_matrigraph_reverse <- function() matrigraph(cantabria, reverse = TRUE)
  expect_snapshot_plot(plot_matrigraph_reverse, "plot_matrigraph_reverse")

  # Rank =======================================================================
  plot_rank_log <- function() plot_rank(cantabria, log = "x")
  expect_snapshot_plot(plot_rank_log, "plot_rank_log")
  plot_rank_nolegend <- function() plot_rank(cantabria, legend = NULL)
  expect_snapshot_plot(plot_rank_nolegend, "plot_rank_nolegend")

  # Spot =======================================================================
  ## Count data
  plot_spot_ring <- function() plot_spot(pueblo, type = "ring")
  expect_snapshot_plot(plot_spot_ring, "plot_spot_ring")
  plot_spot_plain <- function() plot_spot(pueblo, type = "plain")
  expect_snapshot_plot(plot_spot_plain, "plot_spot_plain")

  ## Conditional proportions
  plot_spot_prop1 <- function() plot_spot(pueblo, freq = TRUE, margin = 1)
  expect_snapshot_plot(plot_spot_prop1, "plot_spot_prop1")
  plot_spot_prop2 <- function() plot_spot(pueblo, freq = TRUE, margin = 2)
  expect_snapshot_plot(plot_spot_prop2, "plot_spot_prop2")
}
