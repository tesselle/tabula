test_that("Compute thresholds", {
  skip_if_not_installed("folio")
  data("compiegne", package = "folio")
  counts <- arkhe::as_count(compiegne)

  expect_type(eppm(counts), "double") # matrix
  expect_type(pvi(counts), "double") # matrix
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
  jack <- stats_jackknife(1:10, sum)
  expect_equal(round(jack, 2), c(mean = 49.50, bias = -49.50, error = 8.62))
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
