context("Utilities")

# Test helpers
test_that("Compact", {
  expect_length(compact(is.null, list("A", NULL, "B")), 2)
})
test_that("Detect", {
  expect_type(detect(is.na, c(1, 2, NA, 4, 5, NA)), "logical")
  expect_equal(sum(detect(is.na, c(1, 2, NA, 4, 5, NA))), 2)
})
test_that("Count", {
  expect_type(count(is.na, c(1, 2, NA, 4, 5, NA)), "integer")
  expect_equal(count(is.na, c(1, 2, NA, 4, 5, NA)), 2)
})
test_that("Function composition", {
  expect_type((sum %o% range)(1:5), "integer")
  expect_equal((sum %o% range)(1:5), 6)
})
test_that("NULL OR operator", {
  expect_type(NULL %||% 1, "double")
  expect_equal(NULL %||% 1, 1)
  expect_equal(0 %||% 1, 0)
})
test_that("UUID", {
  id1 <- generate_uuid()
  id2 <- generate_uuid()

  expect_true(is_uuid(id1))
  expect_error(compare_uuid(id1, id2))
})
