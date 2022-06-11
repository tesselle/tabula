# Bertin =======================================================================
test_that("Bertin plot", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("folio")
  data("mississippi", package = "folio")

  # No threshold, no scale
  gg_bertin <- plot_bertin(mississippi)
  vdiffr::expect_doppelganger("bertin", gg_bertin)

  # Threshold, no scale
  gg_bertin_threshold <- plot_bertin(mississippi, threshold = mean)
  vdiffr::expect_doppelganger("bertin_threshold", gg_bertin_threshold)

  # No threshold, scale
  gg_bertin_scale <- plot_bertin(mississippi, scale = scale_01)
  vdiffr::expect_doppelganger("bertin_scale", gg_bertin_scale)
})
# Ford =========================================================================
test_that("Ford plot", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("folio")
  data("mississippi", package = "folio")

  for (i in c(TRUE, FALSE)) {
    gg_ford <- plot_ford(mississippi, EPPM = i)
    vdiffr::expect_doppelganger(paste0("ford_count_EPPM-", i), gg_ford)
  }
})
# Heatmap ======================================================================
test_that("Matrix plot", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("folio")
  data("mississippi", package = "folio")

  test_count <- arkhe::as_count(mississippi)
  test_freq <- arkhe::as_composition(mississippi)
  test_incid <- arkhe::as_incidence(mississippi)

  # Count data
  gg_mtx_count <- plot_heatmap(mississippi)
  vdiffr::expect_doppelganger("mtx_count", gg_mtx_count)

  # Frequency data
  gg_mtx_freq <- plot_heatmap(mississippi, freq = TRUE)
  vdiffr::expect_doppelganger("mtx_freq", gg_mtx_freq)

  # Incidence data
  gg_mtx_incid <- plot_heatmap(mississippi > 0)
  vdiffr::expect_doppelganger("mtx_incid", gg_mtx_incid)
})
# Rank =========================================================================
test_that("Rank plot", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("folio")
  data("mississippi", package = "folio")

  for (i in c(TRUE, FALSE)) {
    # Count data
    gg_rank_count <- plot_rank(mississippi, facet = i)
    vdiffr::expect_doppelganger(paste0("rank_count_facet-", i), gg_rank_count)
  }
  for (j in c("x", "y", "xy", "yx")) {
    # Count data
    gg_rank_log <- plot_rank(mississippi, log = j)
    vdiffr::expect_doppelganger(paste0("rank_count_log-", j), gg_rank_log)
  }
})
# Spot =========================================================================
test_that("Spot plot - Abundance", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("folio")
  data("mississippi", package = "folio")

  # Count data
  gg_spot_count <- plot_spot(mississippi, type = "ring")
  vdiffr::expect_doppelganger("spot_count", gg_spot_count)
  gg_spot_plain_count <- plot_spot(mississippi, type = "plain")
  vdiffr::expect_doppelganger("spot_plain_count", gg_spot_plain_count)
  gg_spot_mean_count <- plot_spot(mississippi, threshold = mean, type = "ring")
  vdiffr::expect_doppelganger("spot_mean_count", gg_spot_mean_count)

  # Frequency data
  gg_spot_freq <- plot_spot(mississippi, type = "ring", freq = TRUE)
  vdiffr::expect_doppelganger("spot_freq", gg_spot_freq)
  gg_spot_mean_freq <- plot_spot(mississippi, threshold = mean, type = "ring", freq = TRUE)
  vdiffr::expect_doppelganger("spot_mean_freq", gg_spot_mean_freq)
  gg_spot_plain_freq <- plot_spot(mississippi, threshold = mean, type = "plain", freq = TRUE)
  vdiffr::expect_doppelganger("spot_plain_freq", gg_spot_plain_freq)
})
