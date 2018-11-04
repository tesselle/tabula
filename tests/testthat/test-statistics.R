context("Statistics")
library(tabula)
options("verbose" = TRUE)

test_count <- as(compiegne, "CountMatrix")
test_freq <- as(compiegne, "FrequencyMatrix")

test_that("Compute thresholds", {
  expect_is(threshold(test_count), "matrix")
  expect_is(threshold(test_freq), "matrix")
})


