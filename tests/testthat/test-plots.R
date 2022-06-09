test_that("Bertin plot", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("folio")
  data("mississippi", package = "folio")
  test_count <- arkhe::as_count(mississippi)

  # Count data
  # No threshold, no scale
  gg_bertin <- plot_bertin(test_count)
  vdiffr::expect_doppelganger("bertin", gg_bertin)
  # Threshold, no scale
  gg_bertin_threshold <- plot_bertin(test_count, threshold = mean)
  vdiffr::expect_doppelganger("bertin_threshold", gg_bertin_threshold)
  # No threshold, scale
  gg_bertin_scale <- plot_bertin(test_count, scale = scale_01)
  vdiffr::expect_doppelganger("bertin_scale", gg_bertin_scale)
})
test_that("Ford plot", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("folio")
  data("mississippi", package = "folio")
  test_count <- arkhe::as_count(mississippi)

  # Count data
  for (i in c(TRUE, FALSE)) {
    gg_ford <- plot_ford(test_count, EPPM = i)
    vdiffr::expect_doppelganger(paste0("ford_count_EPPM-", i), gg_ford)
  }
})
test_that("Matrix plot", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("folio")
  data("mississippi", package = "folio")
  test_count <- arkhe::as_count(mississippi)
  test_freq <- arkhe::as_composition(mississippi)
  test_incid <- arkhe::as_incidence(mississippi)

  # Count data
  gg_mtx_count <- autoplot(test_count)
  vdiffr::expect_doppelganger("mtx_count", gg_mtx_count)
  # Frequency data
  gg_mtx_freq <- autoplot(test_freq)
  vdiffr::expect_doppelganger("mtx_freq", gg_mtx_freq)
  # Incidence data
  gg_mtx_incid <- autoplot(test_incid)
  vdiffr::expect_doppelganger("mtx_incid", gg_mtx_incid)
})
test_that("Rank plot", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("folio")
  data("mississippi", package = "folio")
  test_count <- arkhe::as_count(mississippi)
  test_freq <- arkhe::as_composition(mississippi)

  for (i in c(TRUE, FALSE)) {
    # Count data
    gg_rank_count <- plot_rank(test_count, facet = i)
    vdiffr::expect_doppelganger(paste0("rank_count_facet-", i), gg_rank_count)
    # Frequency data
    gg_rank_freq <- plot_rank(test_freq, facet = i)
    vdiffr::expect_doppelganger(paste0("rank_freq_facet-", i), gg_rank_freq)
  }
  for (j in c("x", "y", "xy", "yx")) {
    # Count data
    gg_rank_log <- plot_rank(test_count, log = j)
    vdiffr::expect_doppelganger(paste0("rank_count_log-", j), gg_rank_log)
  }
})
test_that("Spot plot - Abundance", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("folio")
  data("mississippi", package = "folio")
  test_count <- arkhe::as_count(mississippi)
  test_freq <- arkhe::as_composition(mississippi)

  # Count data
  gg_spot_count <- plot_spot(test_count, type = "ring")
  vdiffr::expect_doppelganger("spot_count", gg_spot_count)
  gg_spot_plain_count <- plot_spot(test_count, type = "plain")
  vdiffr::expect_doppelganger("spot_plain_count", gg_spot_plain_count)
  gg_spot_mean_count <- plot_spot(test_count, threshold = mean, type = "ring")
  vdiffr::expect_doppelganger("spot_mean_count", gg_spot_mean_count)

  # Frequency data
  gg_spot_freq <- plot_spot(test_freq, type = "ring")
  vdiffr::expect_doppelganger("spot_freq", gg_spot_freq)
  gg_spot_mean_freq <- plot_spot(test_freq, threshold = mean, type = "ring")
  vdiffr::expect_doppelganger("spot_mean_freq", gg_spot_mean_freq)
  gg_spot_plain_freq <- plot_spot(test_freq, threshold = mean, type = "plain")
  vdiffr::expect_doppelganger("spot_plain_freq", gg_spot_plain_freq)
})
test_that("Plot Similarity", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("folio")
  data("merzbach", package = "folio")
  test_count <- arkhe::as_count(mississippi)
  test_sim <- similarity(test_count, method = "brainerd")

  gg_mtx_sim <- autoplot(test_sim, upper = TRUE)
  vdiffr::expect_doppelganger("mtx_sim", gg_mtx_sim)

  gg_spot_ring_sim <- plot_spot(test_sim, type = "ring")
  vdiffr::expect_doppelganger("spot_ring_sim", gg_spot_ring_sim)

  gg_spot_plain_sim <- plot_spot(test_sim, type = "plain")
  vdiffr::expect_doppelganger("spot_plain_sim", gg_spot_plain_sim)
})
test_that("Plot Co-Occurrence", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("folio")
  data("merzbach", package = "folio")
  test_occ <- arkhe::as_occurrence(mississippi)

  gg_mtx_occ <- autoplot(test_occ, upper = TRUE)
  vdiffr::expect_doppelganger("mtx_occ", gg_mtx_occ)

  gg_spot_occ <- plot_spot(test_occ, upper = FALSE)
  vdiffr::expect_doppelganger("spot_occ", gg_spot_occ)
})
