context("Check")

test_that("type checks return an error", {
  cnd <- catch_conditions(check_type(numeric(), expected = "list"))
  expect_s3_class(cnd[[1]], "error_bad_type")

  cnd <- catch_conditions(check_type(list(), expected = "atomic"))
  expect_s3_class(cnd[[1]], "error_bad_type")

  # cnd <- catch_conditions(check_type(array(), expected = "vector"))
  # expect_s3_class(cnd[[1]], "error_bad_type")

  cnd <- catch_conditions(check_type(character(), expected = "numeric"))
  expect_s3_class(cnd[[1]], "error_bad_type")

  cnd <- catch_conditions(check_type(character(), expected = "integer"))
  expect_s3_class(cnd[[1]], "error_bad_type")

  cnd <- catch_conditions(check_type(integer(), expected = "double"))
  expect_s3_class(cnd[[1]], "error_bad_type")

  cnd <- catch_conditions(check_type(numeric(), expected = "character"))
  expect_s3_class(cnd[[1]], "error_bad_type")

  cnd <- catch_conditions(check_type(numeric(), expected = "logical"))
  expect_s3_class(cnd[[1]], "error_bad_type")
})
test_that("scalar type checks return an error", {
  cnd <- catch_conditions(check_scalar(list(1, 2), expected = "list"))
  expect_s3_class(cnd[[1]], "error_bad_scalar")

  cnd <- catch_conditions(check_scalar(numeric(2), expected = "atomic"))
  expect_s3_class(cnd[[1]], "error_bad_scalar")

  cnd <- catch_conditions(check_scalar(numeric(2), expected = "vector"))
  expect_s3_class(cnd[[1]], "error_bad_scalar")

  cnd <- catch_conditions(check_scalar(numeric(2), expected = "numeric"))
  expect_s3_class(cnd[[1]], "error_bad_scalar")

  cnd <- catch_conditions(check_scalar(integer(2), expected = "integer"))
  expect_s3_class(cnd[[1]], "error_bad_scalar")

  cnd <- catch_conditions(check_scalar(double(2), expected = "double"))
  expect_s3_class(cnd[[1]], "error_bad_scalar")

  cnd <- catch_conditions(check_scalar(character(2), expected = "character"))
  expect_s3_class(cnd[[1]], "error_bad_scalar")

  cnd <- catch_conditions(check_scalar(logical(2), expected = "logical"))
  expect_s3_class(cnd[[1]], "error_bad_scalar")
})
test_that("object property checks return an error", {
  cnd <- catch_conditions(check_length(LETTERS, expected = 10))
  expect_s3_class(cnd[[1]], "error_bad_dimension")

  k <- list(1:10, 1:20)
  cnd <- catch_conditions(check_lengths(k, expected = NULL))
  expect_s3_class(cnd[[1]], "error_bad_dimension")
  cnd <- catch_conditions(check_lengths(k, expected = c(20, 10)))
  expect_s3_class(cnd[[1]], "error_bad_dimension")

  k <- matrix(1, nrow = 10, ncol = 5)
  cnd <- catch_conditions(check_dimension(k, expected = c(5, 10)))
  expect_s3_class(cnd[[1]], "error_bad_dimension")

  k <- vector(mode = "numeric", length = 10)
  cnd <- catch_conditions(check_names(k, expected = NULL))
  expect_s3_class(cnd[[1]], "error_bad_names")
  names(k) <- LETTERS[1:10]
  cnd <- catch_conditions(check_names(k, expected = LETTERS[11:20]))
  expect_s3_class(cnd[[1]], "error_bad_names")

  k <- matrix(1, nrow = 10, ncol = 5)
  cnd <- catch_conditions(check_names(k, expected = NULL, margin = c(1, 2)))
  expect_s3_class(cnd[[1]], "error_bad_names")
  cnd <- catch_conditions(check_names(k, expected = NULL, margin = 1))
  expect_s3_class(cnd[[1]], "error_bad_names")
  cnd <- catch_conditions(check_names(k, expected = NULL, margin = 2))
  expect_s3_class(cnd[[1]], "error_bad_names")
  dimnames(k) <- list(letters[1:10], letters[1:5])
  cnd <- catch_conditions(check_names(k, expected = LETTERS[1:10], margin = 1))
  expect_s3_class(cnd[[1]], "error_bad_names")
  cnd <- catch_conditions(check_names(k, expected = LETTERS[1:5], margin = 2))
  expect_s3_class(cnd[[1]], "error_bad_names")
})
test_that("data checks return an error", {
  k <- sample(c(1, NA), size = 15, replace = TRUE)
  cnd <- catch_conditions(check_missing(k))
  expect_s3_class(cnd[[1]], "error_data_missing")

  k <- sample(c(1, NaN), size = 15, replace = TRUE)
  cnd <- catch_conditions(check_missing(k))
  expect_s3_class(cnd[[1]], "error_data_missing")

  k <- sample(c(1, Inf), size = 15, replace = TRUE)
  cnd <- catch_conditions(check_infinite(k))
  expect_s3_class(cnd[[1]], "error_data_infinite")
})
test_that("numeric data checks return an error", {
  k <- seq(from = -1, to = 10, by = 1)
  cnd <- catch_conditions(check_numbers(k, expected = "positive", strict = FALSE))
  expect_s3_class(cnd[[1]], "error_bad_number")

  k <- seq(from = 0, to = 10, by = 1)
  cnd <- catch_conditions(check_numbers(k, expected = "positive", strict = TRUE))
  expect_s3_class(cnd[[1]], "error_bad_number")

  k <- seq(from = 1, to = 10, by = 0.5)
  cnd <- catch_conditions(check_numbers(k, expected = "whole"))
  expect_s3_class(cnd[[1]], "error_bad_number")

  k <- seq(from = 1, to = 10, by = 0.5)
  cnd <- catch_conditions(check_numbers(k, expected = "whole", tolerance = 0.2))
  expect_s3_class(cnd[[1]], "error_bad_number")

  k <- seq(from = 1, to = 10, by = 1)
  cnd <- catch_conditions(check_numbers(k, expected = "odd"))
  expect_s3_class(cnd[[1]], "error_bad_number")

  cnd <- catch_conditions(check_constant(k))
  expect_s3_class(cnd[[1]], "error_bad_value")
})
test_that("matrix checks return an error", {
  k <- matrix(sample(1:5, 50, TRUE), nrow = 5, ncol = 10)

  cnd <- catch_conditions(check_matrix(k, expected = "square"))
  expect_s3_class(cnd[[1]], "error_bad_matrix")

  cnd <- catch_conditions(check_matrix(k, expected = "symmetric"))
  expect_s3_class(cnd[[1]], "error_bad_matrix")

  cnd <- catch_conditions(check_constant(k))
  expect_s3_class(cnd[[1]], "error_bad_value")
})
test_that("matrix checks return an error", {
  k <- paste0(LETTERS, collapse = "")
  cnd <- catch_conditions(check_uuid(k))
  expect_s3_class(cnd[[1]], "error_bad_uuid")
})
