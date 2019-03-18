context("Statistics")

test_that("Compute thresholds", {
  expect_is(independance(compiegne, "EPPM"), "matrix")
  expect_is(independance(compiegne, "PVI"), "matrix")
  expect_error(independance(LETTERS))
})
test_that("Binomial coefficient", {
  expect_is(combination(4, 3), "numeric")
  expect_is(combination(171, 3), "numeric") # Ramanujan factorial approximation
  expect_error(combination(3, "a"))

  options("verbose" = TRUE)
  expect_message(combination(171, 3), "Ramanujan approximation of x!")
})
test_that("Confidence interval for a proportion", {
  expect_is(confidence(1:10, level = 0.05, type = "norm"), "numeric")
  expect_is(confidence(1:10, level = 0.05, type = "stud"), "numeric")
  expect_equal(length(confidence(1:10)), 10)
  expect_error(confidence(LETTERS))
})
test_that("Jackknife estimation", {
  jack <- jackknife(1:10, sum)
  expect_is(jack, "list")
  expect_is(jack$values, "integer")
  expect_is(jack$bias, "numeric")
  expect_is(jack$error, "numeric")
  expect_equal(length(jack$values), 10)
})
