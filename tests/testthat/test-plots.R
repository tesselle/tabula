context("Plots")
library(tabula)
library(ggplot2)
library(magrittr)
options("verbose" = TRUE)

test_compiegne <- as(compiegne, "CountMatrix")
test_boves <- as(boves, "FrequencyMatrix")

test_that("Bar plot", {
  for (i in TRUE:FALSE) {
    for (j in TRUE:FALSE) {
      for (k in TRUE:FALSE) {
        gg_count <- plotBar(test_compiegne, EPPM = i, center = j, horizontal = k)
        expect_is(gg_count, "ggplot")
        gg_freq <- plotBar(test_boves, EPPM = i, center = j, horizontal = k)
        expect_is(gg_freq, "ggplot")
      }
    }
  }
})
test_that("Matrix plot", {
  for (i in TRUE:FALSE) {
    gg_count <- plotMatrix(test_compiegne, PVI = i)
    expect_is(gg_count, "ggplot")
    gg_freq <- plotMatrix(test_boves, PVI = i)
    expect_is(gg_freq, "ggplot")
  }
})
test_that("Rank plot", {
  for (i in TRUE:FALSE) {
    gg_count <- plotRank(test_compiegne, facet = i)
    expect_is(gg_count, "ggplot")
    gg_freq <- plotRank(test_boves, facet = i)
    expect_is(gg_freq, "ggplot")
  }
})
test_that("Spot plot", {
  for (i in c(mean, median)) {
    gg_count <- plotSpot(test_compiegne, threshold = i)
    expect_is(gg_count, "ggplot")
    gg_freq <- plotSpot(test_boves, threshold = i)
    expect_is(gg_freq, "ggplot")
  }
})

