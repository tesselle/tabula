context("Plots")
options("verbose" = TRUE)

test_count <- as(compiegne, "CountMatrix")
test_freq <- as(compiegne, "FrequencyMatrix")
test_incid <- as(compiegne, "IncidenceMatrix")
test_occ <- as(compiegne, "OccurrenceMatrix")
test_sim <- similarity(test_count)

test_that("Bar plot", {
  for (i in TRUE:FALSE) {
    for (j in TRUE:FALSE) {
      for (k in TRUE:FALSE) {
        # Count data
        gg_count <- plotBar(test_count, EPPM = i, center = j, horizontal = k)
        expect_is(gg_count, "ggplot")
        # Frequency data
        gg_freq <- plotBar(test_freq, EPPM = i, center = j, horizontal = k)
        expect_is(gg_freq, "ggplot")
      }
    }
  }
  # Count data with confidence interval
  expect_is(plotBar(test_count, level = 0.05), "ggplot")
})
test_that("Matrix plot", {
  for (i in TRUE:FALSE) {
    # Count data
    expect_is(plotMatrix(test_count, PVI = i), "ggplot")
    # Frequency data
    expect_is(plotMatrix(test_freq, PVI = i), "ggplot")
  }
  # Incidence data
  expect_is(plotMatrix(test_incid), "ggplot")
})
test_that("Rank plot", {
  for (i in TRUE:FALSE) {
    # Count data
    expect_is(plotRank(test_count, facet = i), "ggplot")
    # Frequency data
    expect_is(plotRank(test_freq, facet = i), "ggplot")
  }
  for (j in c("x", "y", "xy")) {
    # Count data
    expect_is(plotRank(test_count, log = j), "ggplot")
  }
})
test_that("Spot plot", {
  for (i in c(mean, median)) {
    # Count data
    expect_is(plotSpot(test_count, threshold = i), "ggplot")
    # Frequency data
    expect_is(plotSpot(test_freq, threshold = i), "ggplot")
  }
  # Similarity matrix
  expect_is(plotSpot(test_sim), "ggplot")
  # Co-Occurrence matrix
  expect_is(plotSpot(test_occ), "ggplot")
})
