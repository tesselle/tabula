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
  ## No threshold, no scale
  # gg_bertin <- plot_bertin(cantabria)
  # vdiffr::expect_doppelganger("bertin", gg_bertin)

  ## Threshold, no scale
  # gg_bertin_threshold <- plot_bertin(cantabria, threshold = mean)
  # vdiffr::expect_doppelganger("bertin_threshold", gg_bertin_threshold)

  ## No threshold, scale
  # gg_bertin_scale <- plot_bertin(cantabria, scale = scale_01)
  # vdiffr::expect_doppelganger("bertin_scale", gg_bertin_scale)

  # Ford =======================================================================
  # gg_ford <- plot_ford(cantabria)
  # vdiffr::expect_doppelganger("ford", gg_ford)

  # Seriograph =================================================================
  # for (i in c(TRUE, FALSE)) {
  #   gg_seriograph <- seriograph(cantabria, weights = i)
  #   vdiffr::expect_doppelganger(paste0("seriograph_weights-", i), gg_seriograph)
  # }

  # Dice-Leraas ================================================================
  plot_dice_leraas <- function() plot_diceleraas(pueblo)
  expect_snapshot_plot(plot_dice_leraas, "plot_dice_leraas")

  # Heatmap ====================================================================
  ## Count data
  plot_mtx_count <- function() plot_heatmap(cantabria)
  expect_snapshot_plot(plot_mtx_count, "plot_mtx_count")

  ## Frequency data
  plot_mtx_freq <- function() plot_heatmap(cantabria, freq = TRUE)
  expect_snapshot_plot(plot_mtx_freq, "plot_mtx_freq")

  ## Incidence data
  plot_mtx_incid <- function() plot_heatmap(cantabria > 0)
  expect_snapshot_plot(plot_mtx_incid, "plot_mtx_incid")

  # Matrigraph =================================================================
  for (i in c(TRUE, FALSE)) {
    plot_matrigraph <- function() matrigraph(cantabria, reverse = i)
    expect_snapshot_plot(plot_matrigraph, paste0("matrigraph_reverse-", i))
  }

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

  ## Frequency data
  plot_spot_freq_ring <- function() plot_spot(pueblo, type = "ring", freq = TRUE)
  expect_snapshot_plot(plot_spot_freq_ring, "plot_spot_freq_ring")
  plot_spot_freq_plain <- function() plot_spot(pueblo, type = "plain", freq = TRUE)
  expect_snapshot_plot(plot_spot_freq_plain, "plot_spot_freq_plain")
}
