context("Statistics")

test_that("Compute thresholds", {
  expect_type(independance(compiegne, "EPPM"), "double") # matrix
  expect_type(independance(compiegne, "PVI"), "double") # matrix
  expect_error(independance(LETTERS))
})
test_that("Binomial coefficient", {
  expect_type(combination(4, 3), "double")
  expect_type(combination(171, 3), "double") # Ramanujan factorial approx.
  expect_error(combination(3, "a"))

  options("verbose" = TRUE)
  expect_message(combination(171, 3), "Ramanujan approximation of x!")
})
test_that("Confidence interval for a proportion", {
  expect_type(confidence(1:10, level = 0.05, type = "norm"), "double")
  expect_type(confidence(1:10, level = 0.05, type = "stud"), "double")
  expect_length(confidence(1:10), 10)
  expect_error(confidence(LETTERS))
})
test_that("Jackknife estimation", {
  jack <- jackknife(1:10, sum)
  expect_type(jack, "list")
  expect_type(jack$values, "double")
  expect_type(jack$bias, "double")
  expect_type(jack$error, "double")
  expect_type(jack$values, "double")
  expect_equal(jack$values, c(54, 53, 52, 51, 50, 49, 48, 47, 46, 45))
  expect_length(jack$values, 10)
})
