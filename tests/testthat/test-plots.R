context("Plots")

test_count <- as(mississippi, "CountMatrix")
test_freq <- as(mississippi, "AbundanceMatrix")
test_incid <- as(mississippi, "IncidenceMatrix")
test_occ <- as(mississippi, "OccurrenceMatrix")
test_sim <- similarity(test_count)

test_that("Deprecated plot", {
  # Deprecated
  gg_old_bar1 <- suppressWarnings(
    plotBar(test_count, level = FALSE, EPPM = FALSE,
            center = TRUE, horizontal = FALSE)
  )
  vdiffr::expect_doppelganger("gg_old_bar1", gg_old_bar1)
  gg_old_bar2 <- suppressWarnings(
    plotBar(test_count, level = 0.95, EPPM = FALSE,
            center = TRUE, horizontal = FALSE)
  )
  vdiffr::expect_doppelganger("gg_old_bar2", gg_old_bar2)
  gg_old_bar3 <- suppressWarnings(
    plotBar(test_count, level = 0.95, EPPM = TRUE,
            center = TRUE, horizontal = FALSE)
  )
  vdiffr::expect_doppelganger("gg_old_bar3", gg_old_bar3)
  gg_old_bar4 <- suppressWarnings(
    plotBar(test_count, level = FALSE, EPPM = TRUE,
            center = FALSE, horizontal = TRUE)
  )
  vdiffr::expect_doppelganger("gg_old_bar4", gg_old_bar4)

  expect_warning(plotBar(test_count), "deprecated")
  expect_warning(plotBar(test_freq), "deprecated")
  expect_warning(plotMatrix(test_count), "deprecated")
  expect_warning(plotRank(test_count), "deprecated")
  expect_warning(plotSpot(test_count), "deprecated")
})
test_that("Bertin plot", {
  # Count data
  # No threshold, no scale
  gg_bertin <- plot_bertin(test_count)
  vdiffr::expect_doppelganger("bertin", gg_bertin)
  # Threshold, no scale
  gg_bertin_threshold <- plot_bertin(test_count, threshold = mean)
  vdiffr::expect_doppelganger("bertin_threshold", gg_bertin_threshold)
  # No threshold, scale
  scale_01 <- function(x) (x - min(x)) / (max(x) - min(x))
  gg_bertin_scale <- plot_bertin(test_count, scale = scale_01)
  vdiffr::expect_doppelganger("bertin_scale", gg_bertin_scale)
})
test_that("Ford plot", {
  # Count data
  for (i in c(TRUE, FALSE)) {
    # Count data
    gg_ford <- plot_ford(test_count, EPPM = i)
    vdiffr::expect_doppelganger(paste0("ford_count_EPPM-", i), gg_ford)
  }
})
test_that("Matrix plot", {
  for (i in c(TRUE, FALSE)) {
    # Count data
    gg_mtx_count <- plot_heatmap(test_count, PVI = i)
    vdiffr::expect_doppelganger(paste0("mtx_count_PVI-", i), gg_mtx_count)
  }
  # Frequency data
  gg_mtx_freq <- plot_heatmap(test_freq)
  vdiffr::expect_doppelganger("mtx_freq", gg_mtx_freq)
  # Incidence data
  gg_mtx_incid <- plot_heatmap(test_incid)
  vdiffr::expect_doppelganger("mtx_incid", gg_mtx_incid)
})
test_that("Rank plot", {
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
  # Count data, no threshold
  gg_spot_count <- plot_spot(test_count)
  vdiffr::expect_doppelganger("spot_count", gg_spot_count)
  # Frequency data
  gg_spot_freq <- plot_spot(test_freq, threshold = mean)
  vdiffr::expect_doppelganger("spot_freq", gg_spot_freq)
})
test_that("Spot plot - Similarity", {
  gg_spot_sim <- plot_spot(test_sim)
  vdiffr::expect_doppelganger("spot_sim", gg_spot_sim)
})
test_that("Spot plot - Co-Occurrence", {
  gg_spot_occ <- plot_spot(test_occ)
  vdiffr::expect_doppelganger("spot_occ", gg_spot_occ)
})
