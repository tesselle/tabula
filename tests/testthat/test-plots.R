# Bertin =======================================================================
test_that("Bertin plot", {
  skip_if_not_installed("vdiffr")
  data("cantabria")

  # No threshold, no scale
  gg_bertin <- plot_bertin(cantabria)
  vdiffr::expect_doppelganger("bertin", gg_bertin)

  # Threshold, no scale
  gg_bertin_threshold <- plot_bertin(cantabria, threshold = mean)
  vdiffr::expect_doppelganger("bertin_threshold", gg_bertin_threshold)

  # No threshold, scale
  gg_bertin_scale <- plot_bertin(cantabria, scale = scale_01)
  vdiffr::expect_doppelganger("bertin_scale", gg_bertin_scale)
})
# Ford =========================================================================
test_that("Ford plot", {
  skip_if_not_installed("vdiffr")
  data("cantabria")

  gg_ford <- plot_ford(cantabria)
  vdiffr::expect_doppelganger("ford", gg_ford)
})
test_that("Seriograph", {
  skip_if_not_installed("vdiffr")
  data("cantabria")

  for (i in c(TRUE, FALSE)) {
    gg_seriograph <- seriograph(cantabria, weights = i)
    vdiffr::expect_doppelganger(paste0("seriograph_weights-", i), gg_seriograph)
  }
})
# Dice-Leraas ==================================================================
test_that("Dice-Leraas plot", {
  skip_if_not_installed("vdiffr")
  data("cantabria")

  gg_diceleraas <- plot_diceleraas(cantabria)
  vdiffr::expect_doppelganger("diceleraas", gg_diceleraas)
})
# Heatmap ======================================================================
test_that("Matrix plot", {
  skip_if_not_installed("vdiffr")
  data("cantabria")

  # Count data
  gg_mtx_count <- plot_heatmap(cantabria)
  vdiffr::expect_doppelganger("mtx_count", gg_mtx_count)

  # Frequency data
  gg_mtx_freq <- plot_heatmap(cantabria, freq = TRUE)
  vdiffr::expect_doppelganger("mtx_freq", gg_mtx_freq)

  # Incidence data
  gg_mtx_incid <- plot_heatmap(cantabria > 0)
  vdiffr::expect_doppelganger("mtx_incid", gg_mtx_incid)
})
test_that("Matrigraph", {
  skip_if_not_installed("vdiffr")
  data("cantabria")

  for (i in c(TRUE, FALSE)) {
    gg_matrigraph <- matrigraph(cantabria, reverse = i)
    vdiffr::expect_doppelganger(paste0("matrigraph_revese-", i), gg_matrigraph)
  }
})
# Rank =========================================================================
test_that("Rank plot", {
  skip_if_not_installed("vdiffr")
  data("cantabria")

  for (i in c(TRUE, FALSE)) {
    # Count data
    gg_rank_count <- plot_rank(cantabria, facet = i)
    vdiffr::expect_doppelganger(paste0("rank_count_facet-", i), gg_rank_count)
  }
  for (j in c("x", "y", "xy", "yx")) {
    # Count data
    gg_rank_log <- plot_rank(cantabria, log = j)
    vdiffr::expect_doppelganger(paste0("rank_count_log-", j), gg_rank_log)
  }
})
# Spot =========================================================================
test_that("Spot plot", {
  skip_if_not_installed("vdiffr")
  data("cantabria")

  # Count data
  gg_spot_count <- plot_spot(cantabria, type = "ring")
  vdiffr::expect_doppelganger("spot_count", gg_spot_count)
  gg_spot_plain_count <- plot_spot(cantabria, type = "plain")
  vdiffr::expect_doppelganger("spot_plain_count", gg_spot_plain_count)
  gg_spot_mean_count <- plot_spot(cantabria, threshold = mean, type = "ring")
  vdiffr::expect_doppelganger("spot_mean_count", gg_spot_mean_count)

  # Frequency data
  gg_spot_freq <- plot_spot(cantabria, type = "ring", freq = TRUE)
  vdiffr::expect_doppelganger("spot_freq", gg_spot_freq)
  gg_spot_mean_freq <- plot_spot(cantabria, threshold = mean, type = "ring", freq = TRUE)
  vdiffr::expect_doppelganger("spot_mean_freq", gg_spot_mean_freq)
  gg_spot_plain_freq <- plot_spot(cantabria, threshold = mean, type = "plain", freq = TRUE)
  vdiffr::expect_doppelganger("spot_plain_freq", gg_spot_plain_freq)
})
