context("Statistics")

test_that("Compute thresholds", {
  expect_type(independance(compiegne, "EPPM"), "double") # matrix
  expect_type(independance(compiegne, "PVI"), "double") # matrix
  expect_error(independance(LETTERS))
})
test_that("Binomial coefficient", {
  expect_equal(combination(4, 3), 4)
  # Ramanujan factorial approx.
  expect_equal(combination(171, 3), 818816.247275706)
  expect_error(combination(3, "a"))

  options("verbose" = TRUE)
  expect_message(combination(171, 3), "Ramanujan approximation of x!")
})
test_that("Confidence interval for a proportion", {
  expect_type(confidence_proportion(1:10, alpha = 0.05, type = "norm"), "double")
  expect_type(confidence_proportion(1:10, alpha = 0.05, type = "stud"), "double")
  expect_length(confidence_proportion(1:10), 10)
  expect_error(confidence_proportion(LETTERS))
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
test_that("FIT test", {
  ## Data from Feder et al. 2014 (table S2)
  test_fit <- FIT(
    v = c(73/93, 98/105, 97/99, 97/98),
    t = c(665, 745, 825, 910)
  )
  expect_equal(round(test_fit[[1]], 2), 2.42)
  expect_equal(round(test_fit[[2]], 2), 0.14)
})
