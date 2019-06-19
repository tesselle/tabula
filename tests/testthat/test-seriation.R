context("Seriation")
options("verbose" = FALSE)


test_that("Reciprocal averaging", {
  count <- as(compiegne, "CountMatrix")

  indices_row <- seriate(count, method = "reciprocal", margin = 1)
  expect_equal(indices_row@rows, c(1, 2, 5, 3, 4))
  expect_equal(indices_row@columns, 1:16)
  expect_equal(indices_row@id, count@id)

  indices_col <- seriate(count, method = "reciprocal", margin = 2)
  expect_equal(indices_col@rows, 1:5)
  expect_equal(indices_col@columns,
               c(14, 1, 11, 3, 16, 12, 5, 2, 15, 13, 4, 7, 6, 9, 10, 8))
  expect_equal(indices_col@id, count@id)

  expect_warning(seriate(count, method = "reciprocal", stop = 1, margin = 2))
})
test_that("Reciprocal averaging on EPPM", {
  count <- as(compiegne, "CountMatrix")
  indices <- seriate(count, method = "reciprocal", EPPM = TRUE, margin = 2)
  expected <- c("N", "A", "C", "K", "P", "L", "B", "E",
                "I", "M", "D", "G", "O", "J", "F", "H")
  expect_equal(LETTERS[indices@columns], expected)
})
test_that("Reciprocal ranking", {
  incid <- as(compiegne, "IncidenceMatrix")

  indices <- seriate(incid, method = "reciprocal", margin = c(1, 2))
  expect_equal(indices@rows, c(1, 2, 3, 4, 5))
  expect_equal(indices@columns,
               c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 14, 15, 13, 16, 9))

  expect_s4_class(indices, "PermutationOrder")
  expect_s4_class(permute(incid, indices), "IncidenceMatrix")
  expect_equal(permute(incid, indices)@id, incid@id)
})
test_that("Correspondance Analysis", {
  count <- as(compiegne, "CountMatrix")

  indices <- seriate(count, method = "correspondance", margin = c(1, 2))
  expect_equal(indices@rows, c(1, 2, 3, 4, 5))
  expect_equal(indices@columns,
               c(14, 11, 1, 12, 3, 16, 5, 2, 15, 13, 7, 4, 6, 10, 9, 8))

  expect_s4_class(indices, "PermutationOrder")
  expect_s4_class(permute(count, indices), "CountMatrix")
  expect_equal(permute(count, indices)@id, count@id)
})
test_that("Refined correspondance Analysis", {
  count <- with_seed(
    12345,
    CountMatrix(sample(1:100, 100, TRUE), ncol = 10,
                dimnames = list(LETTERS[1:10], LETTERS[26:17]))
  )

  # Define cutoff as one standard deviation above the mean
  fun <- function(x) { mean(x) + sd(x) }
  subset <- with_seed(12345, refine(count, cutoff = fun))

  expect_s4_class(subset, "BootCA")
  expect_equal(subset@cutoff, c(0.3537187, 0.3231214), tolerance = 1e-7)
  expect_equal(subset@keep[[1]], c(2, 3, 4, 6, 7, 8, 9, 10))
  expect_equal(subset@keep[[2]], c(1, 2, 4, 5, 6, 8, 10))
  expect_equal(subset@id, count@id)

  indices <- seriate(count, subset)
  expect_s4_class(indices, "PermutationOrder")
  expect_s4_class(permute(count, indices), "CountMatrix")
})


