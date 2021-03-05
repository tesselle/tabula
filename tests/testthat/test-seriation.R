test_that("Reciprocal Ranking - Counts", {
  skip_on_cran()
  skip_if_not_installed("folio")
  data("compiegne", package = "folio")
  counts <- arkhe::as_count(compiegne)

  ## Permute rows
  indices_row <- seriate_rank(counts, margin = 1)
  exp_row <- c(1, 2, 5, 3, 4)
  expect_equal(indices_row@rows, exp_row)
  expect_equal(indices_row@columns, 1:16)

  ## Permute columns
  indices_col <- seriate_rank(counts, margin = 2)
  exp_col <- c(14, 1, 11, 3, 16, 12, 5, 2, 15, 13, 4, 7, 6, 9, 10, 8)
  expect_equal(indices_col@rows, 1:5)
  expect_equal(indices_col@columns, exp_col)
  expect_type(get_order(indices_col), "list")

  expect_s4_class(permute(counts, indices_col), "CountMatrix")
  expect_warning(seriate_rank(counts, stop = 1, margin = 2))
})
test_that("Reciprocal Ranking on EPPM - Counts", {
  skip_on_cran()
  skip_if_not_installed("folio")
  data("compiegne", package = "folio")
  counts <- arkhe::as_count(compiegne)

  indices <- seriate_rank(counts, EPPM = TRUE, margin = 2)

  expected <- c("N", "A", "C", "K", "P", "L", "B", "E",
                "I", "M", "D", "G", "O", "J", "F", "H")
  expect_equal(LETTERS[indices@columns], expected)
})
test_that("Reciprocal Ranking - Incidence", {
  skip_on_cran()
  skip_if_not_installed("folio")
  data("compiegne", package = "folio")
  incid <- arkhe::as_incidence(compiegne)

  indices <- seriate_rank(incid, margin = c(1, 2))

  exp_row <- c(1, 2, 3, 4, 5)
  exp_col <- c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 14, 15, 13, 16, 9)
  expect_equal(indices@rows, exp_row)
  expect_equal(indices@columns, exp_col)

  expect_s4_class(permute(incid, indices), "IncidenceMatrix")
})
test_that("Average Ranking", {
  skip_on_cran()
  skip_if_not_installed("folio")
  data("compiegne", package = "folio")
  counts <- arkhe::as_count(compiegne)

  indices <- seriate_average(counts, margin = c(1, 2))

  exp_row <- c(1, 2, 3, 4, 5)
  exp_col <- c(14, 11, 1, 12, 3, 16, 5, 2, 15, 13, 7, 4, 6, 10, 9, 8)
  expect_equal(indices@rows, exp_row)
  expect_equal(indices@columns, exp_col)

  expect_s4_class(permute(counts, indices), "CountMatrix")

  counts2 <- arkhe::as_count(merzbach)
  expect_error(seriate_average(counts2), "Empty columns detected.")
})
test_that("Refined Correspondence Analysis", {
  skip_on_cran()
  skip_if_not_installed("folio")
  data("zuni", package = "folio")
  counts <- arkhe::as_count(zuni)

  ## Define cutoff as one standard deviation above the mean
  fun <- function(x) { mean(x) + sd(x) }
  corresp <- dimensio::ca(counts)
  sub <- with_seed(12345, refine_seriation(corresp, cutoff = fun, n = 30))

  ## /!\ Fails on noLD platforms unless a tolerance is set in expect_equal
  eps <- if (capabilities("long.double")) .Machine$double.eps^0.5 else 0.1
  expect_equal(
    object = round(sub@cutoff, 3),
    expected = c(1.781, 0.246),
    ignore_attr = TRUE,
    tolerance = eps
  )
})
