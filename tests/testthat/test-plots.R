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
  gg_bertin_count <- plotBertin(test_count, threshold = mean)
  expect_s3_class(gg_bertin_count, "ggplot")
  vdiffr::expect_doppelganger("gg_bertin_count", gg_bertin_count)
})
test_that("Ford plot", {
  # Count data
  gg_ford <- vector(mode = "list", length = 4)
  for (i in TRUE:FALSE) {
    # Count data
    gg_ford[[i + 1]] <- plotFord(test_count, EPPM = i)
    expect_s3_class(gg_ford[[i + 1]], "ggplot")
    vdiffr::expect_doppelganger(paste0("gg_ford_count_EPPM-", i),
                                gg_ford[[i + 1]])
  }
})
test_that("Matrix plot", {
  gg_mtx <- vector(mode = "list", length = 4)
  for (i in TRUE:FALSE) {
    # Count data
    gg_mtx[[i + 1]] <- plotMatrix(test_count, PVI = i)
    expect_s3_class(gg_mtx[[i + 1]], "ggplot")
    vdiffr::expect_doppelganger(paste0("gg_mtx_count_PVI-", i), gg_mtx[[i + 1]])
    # Frequency data
    gg_mtx[[i + 3]] <- plotMatrix(test_freq, PVI = i)
    expect_s3_class(gg_mtx[[i + 3]], "ggplot")
    vdiffr::expect_doppelganger(paste0("gg_mtx_freq_PVI-", i), gg_mtx[[i + 3]])
  }
  # Incidence data
  gg_mtx_incid <- plotMatrix(test_incid)
  expect_s3_class(gg_mtx_incid, "ggplot")
  vdiffr::expect_doppelganger("gg_mtx_incid", gg_mtx_incid)
})
test_that("Rank plot", {
  gg_rank <- vector(mode = "list", length = 4)
  for (i in TRUE:FALSE) {
    # Count data
    gg_rank[[i + 1]] <- plotRank(test_count, facet = i)
    expect_s3_class(gg_rank[[i + 1]], "ggplot")
    vdiffr::expect_doppelganger(paste0("gg_rank_count_facet-", i),
                                gg_rank[[i + 1]])
    # Frequency data
    gg_rank[[i + 3]] <- plotRank(test_freq, facet = i)
    expect_s3_class(gg_rank[[i + 3]], "ggplot")
    vdiffr::expect_doppelganger(paste0("gg_rank_freq_facet-", i),
                                gg_rank[[i + 3]])
  }
  k <- 1
  for (j in c("x", "y", "xy", "yx")) {
    # Count data
    gg_rank[[k]] <- plotRank(test_count, log = j)
    expect_s3_class(gg_rank[[k]], "ggplot")
    vdiffr::expect_doppelganger(paste0("gg_rank_count_log-", j), gg_rank[[k]])
    k <- k + 1
  }
})
test_that("Spot plot - Abundance", {
  # Count data
  gg_spot_count <- plotSpot(test_count, threshold = mean)
  expect_s3_class(gg_spot_count, "ggplot")
  vdiffr::expect_doppelganger("gg_spot_count", gg_spot_count)
  # Frequency data
  gg_spot_freq <- plotSpot(test_freq, threshold = mean)
  expect_s3_class(gg_spot_freq, "ggplot")
  vdiffr::expect_doppelganger("gg_spot_freq", gg_spot_freq)
})
test_that("Spot plot - Similarity", {
  gg_spot_sim <- plotSpot(test_sim)
  expect_s3_class(gg_spot_sim, "ggplot")
  vdiffr::expect_doppelganger("gg_spot_sim", gg_spot_sim)
})
test_that("Spot plot - Co-Occurrence", {
  gg_spot_occ <- plotSpot(test_occ)
  expect_s3_class(gg_spot_occ, "ggplot")
  vdiffr::expect_doppelganger("gg_spot_occ", gg_spot_occ)
})
