context("Seriation")
options("verbose" = FALSE)


test_that("Reciprocal averaging", {
  count <- as(compiegne, "CountMatrix")

  indices_row <- seriate_reciprocal(count, margin = 1)
  expect_equal(indices_row@rows, c(1, 2, 5, 3, 4))
  expect_equal(indices_row@columns, 1:16)
  expect_equal(indices_row@id, count@id)

  indices_col <- seriate_reciprocal(count, margin = 2)
  expect_equal(indices_col@rows, 1:5)
  expect_equal(indices_col@columns,
               c(14, 1, 11, 3, 16, 12, 5, 2, 15, 13, 4, 7, 6, 9, 10, 8))
  expect_equal(indices_col@id, count@id)

  expect_warning(seriate_reciprocal(count, stop = 1, margin = 2))
})
test_that("Reciprocal averaging on EPPM", {
  count <- as(compiegne, "CountMatrix")
  indices <- seriate_reciprocal(count, EPPM = TRUE, margin = 2)
  expected <- c("N", "A", "C", "K", "P", "L", "B", "E",
                "I", "M", "D", "G", "O", "J", "F", "H")
  expect_equal(LETTERS[indices@columns], expected)
})
test_that("Reciprocal ranking", {
  incid <- as(compiegne, "IncidenceMatrix")

  indices <- seriate_reciprocal(incid, margin = c(1, 2))
  expect_equal(indices@rows, c(1, 2, 3, 4, 5))
  expect_equal(indices@columns,
               c(1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 14, 15, 13, 16, 9))

  expect_s4_class(indices, "PermutationOrder")
  expect_s4_class(permute(incid, indices), "IncidenceMatrix")
  expect_equal(permute(incid, indices)@id, incid@id)
})
test_that("correspondence Analysis", {
  count <- as(compiegne, "CountMatrix")

  indices <- seriate_correspondence(count, margin = c(1, 2))
  expect_equal(indices@rows, c(1, 2, 3, 4, 5))
  expect_equal(indices@columns,
               c(14, 11, 1, 12, 3, 16, 5, 2, 15, 13, 7, 4, 6, 10, 9, 8))

  expect_s4_class(indices, "PermutationOrder")
  expect_s4_class(permute(count, indices), "CountMatrix")
  expect_equal(permute(count, indices)@id, count@id)

  count2 <- as(merzbach, "CountMatrix")
  expect_warning(seriate_correspondence(count2))
})
# test_that("Refined correspondence Analysis", {
  # count <- as(zuni, "CountMatrix")

  # Define cutoff as one standard deviation above the mean
  # fun <- function(x) { mean(x) + sd(x) }
  # expect_warning(refine(count, cutoff = fun), "deprecated")
  # subset <- with_seed(12345, refine_seriation(count, cutoff = fun))

  # expect_s4_class(subset, "BootCA")
  # /!\ Fails on noLD platforms unless a tolerance is set in expect_equal
  # eps <- if (capabilities("long.double")) .Machine$double.eps^0.5 else 0.1
  # expect_equal(round(subset@cutoff, 3), c(2.243, 0.379), tolerance = eps)
  # expect_equal(subset@id, count@id)
  #
  # indices <- seriate_correspondence(count, subset, margin = 1)
  # expect_s4_class(indices, "PermutationOrder")
  # expect_s4_class(permute(count, indices), "CountMatrix")
# })


