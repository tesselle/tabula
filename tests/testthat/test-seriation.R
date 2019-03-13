context("Seriation")
options("verbose" = FALSE)

test_that("Reciprocal averaging on EPPM", {
  count <- as(compiegne, "CountMatrix")
  indices <- seriate(count, method = "reciprocal", EPPM = TRUE, margin = 2)
  expected <- c("N", "A", "C", "K", "P", "L", "B", "E",
                "I", "M", "D", "G", "O", "J", "F", "H")
  expect_identical(LETTERS[indices@columns], expected)
})
test_that("Reciprocal averaging", {
  count <- as(compiegne, "CountMatrix")
  expect_warning(seriate(count, method = "reciprocal", stop = 1, margin = 2))
})
test_that("Reciprocal ranking", {
  incid <- as(compiegne, "IncidenceMatrix")
  indices <- seriate(incid, method = "reciprocal", margin = 2)
  expect_s4_class(indices, "PermutationOrder")
  expect_s4_class(permute(incid, indices), "IncidenceMatrix")
})
test_that("Correspondance Analysis", {
  count <- as(compiegne, "CountMatrix")
  indices <- seriate(count, method = "correspondance")
  expect_s4_class(indices, "PermutationOrder")
  expect_s4_class(permute(count, indices), "CountMatrix")
})
test_that("Refined correspondance Analysis", {
  count <- as(compiegne, "CountMatrix")
  # Define cutoff as one standard deviation above the mean
  fun <- function(x) { mean(x) + sd(x) }
  subset <- refine(count, cutoff = fun)
  expect_s4_class(subset, "BootCA")
  indices <- seriate(count, subset)
  expect_s4_class(indices, "PermutationOrder")
  expect_s4_class(permute(count, indices), "CountMatrix")
})


