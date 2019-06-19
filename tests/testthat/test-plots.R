context("Plots")

test_count <- as(mississippi, "CountMatrix")
test_freq <- as(mississippi, "FrequencyMatrix")
test_incid <- as(mississippi, "IncidenceMatrix")
test_occ <- as(mississippi, "OccurrenceMatrix")
test_sim <- similarity(test_count)

test_that("Bar plot", {
  # Deprecated
  expect_warning(plotBar(test_count), "deprecated")
  expect_warning(plotBar(test_freq), "deprecated")
})
test_that("Bertin plot", {
  # Count data
  # No threshold, no scale
  gg_bertin <- plotBertin(test_count)
  vdiffr::expect_doppelganger("bertin", gg_bertin)
  # Threshold, no scale
  gg_bertin_threshold <- plotBertin(test_count, threshold = mean)
  vdiffr::expect_doppelganger("bertin_threshold", gg_bertin_threshold)
  # No threshold, scale
  scale_01 <- function(x) (x - min(x)) / (max(x) - min(x))
  gg_bertin_scale <- plotBertin(test_count, scale = scale_01)
  vdiffr::expect_doppelganger("bertin_scale", gg_bertin_scale)
})
test_that("Ford plot", {
  # Count data
  for (i in c(TRUE, FALSE)) {
    # Count data
    gg_ford <- plotFord(test_count, EPPM = i)
    vdiffr::expect_doppelganger(paste0("ford_count_EPPM-", i), gg_ford)
  }
})
test_that("Matrix plot", {
  for (i in c(TRUE, FALSE)) {
    # Count data
    gg_mtx_count <- plotMatrix(test_count, PVI = i)
    vdiffr::expect_doppelganger(paste0("mtx_count_PVI-", i), gg_mtx_count)
    # Frequency data
    gg_mtx_freq <- plotMatrix(test_freq, PVI = i)
    vdiffr::expect_doppelganger(paste0("mtx_freq_PVI-", i), gg_mtx_freq)
  }
  # Incidence data
  gg_mtx_incid <- plotMatrix(test_incid)
  vdiffr::expect_doppelganger("mtx_incid", gg_mtx_incid)
})
test_that("Rank plot", {
  for (i in c(TRUE, FALSE)) {
    # Count data
    gg_rank_count <- plotRank(test_count, facet = i)
    vdiffr::expect_doppelganger(paste0("rank_count_facet-", i), gg_rank_count)
    # Frequency data
    gg_rank_freq <- plotRank(test_freq, facet = i)
    vdiffr::expect_doppelganger(paste0("rank_freq_facet-", i), gg_rank_freq)
  }
  for (j in c("x", "y", "xy", "yx")) {
    # Count data
    gg_rank_log <- plotRank(test_count, log = j)
    vdiffr::expect_doppelganger(paste0("rank_count_log-", j), gg_rank_log)
  }
})
test_that("Spot plot - Abundance", {
  # Count data, no threshold
  gg_spot_count <- plotSpot(test_count)
  vdiffr::expect_doppelganger("spot_count", gg_spot_count)
  # Frequency data
  gg_spot_freq <- plotSpot(test_freq, threshold = mean)
  vdiffr::expect_doppelganger("spot_freq", gg_spot_freq)
})
test_that("Spot plot - Similarity", {
  gg_spot_sim <- plotSpot(test_sim)
  vdiffr::expect_doppelganger("spot_sim", gg_spot_sim)
})
test_that("Spot plot - Co-Occurrence", {
  gg_spot_occ <- plotSpot(test_occ)
  vdiffr::expect_doppelganger("spot_occ", gg_spot_occ)
})
test_that("Time plot", {
  # Keep only decoration types that have a maximum frequency of at least 50
  keep <- apply(X = merzbach, MARGIN = 2, FUN = function(x) max(x) >= 50)
  count_merzbach <- as(merzbach[, keep], "CountMatrix")
  # Use the row names as time coordinates (roman numerals)
  setDates(count_merzbach) <- rownames(merzbach)
  # Plot time vs abundance
  gg_time <- plotTime(count_merzbach)
  vdiffr::expect_doppelganger("time", gg_time)
  # Plot time vs abundance and highlight selection
  gg_time_FIT <- plotTime(count_merzbach, highlight = "FIT", roll = TRUE)
  vdiffr::expect_doppelganger("time_FIT", gg_time_FIT)
})
