context("Statistics")
library(tabula)
options("verbose" = TRUE)

test_count <- as(compiegne, "CountMatrix")
test_freq <- as(compiegne, "FrequencyMatrix")

test_that("Compute thresholds", {
  expect_is(independance(test_count, "EPPM"), "matrix")
  expect_is(independance(test_count, "PVI"), "matrix")
})


