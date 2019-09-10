context("Predicates")

test_that("utility predicates return a logical scalar", {
  expect_true(is_empty(numeric(0)))
  expect_false(is_empty(numeric(1)))

  A <- LETTERS
  B <- c("D", "E", "F")
  b <- c("d", "e", "f")
  expect_true(is_subset(B, A))
  expect_true(is_subset(LETTERS, LETTERS))
  expect_false(is_subset(A, B))
  expect_false(is_subset(A, b))
})
test_that("type predicates return a logical scalar", {
  expect_true(is_list(list()))
  expect_false(is_list(numeric(1)))

  expect_true(is_atomic(numeric(1)))
  expect_false(is_atomic(list()))

  expect_true(is_vector(numeric(1)))
  expect_true(is_vector(list()))
  expect_false(is_vector(expression()))

  expect_true(is_numeric(1L))
  expect_true(is_numeric(1.5))
  expect_false(is_numeric("A"))

  expect_true(is_integer(1L))
  expect_false(is_integer(1.5))

  expect_true(is_double(1.5))
  expect_false(is_double(1L))

  expect_true(is_character("A"))
  expect_false(is_character(1))

  expect_true(is_logical(TRUE))
  expect_false(is_logical(1))

  expect_true(is_error(try(log("X"), silent = TRUE)))
  expect_false(is_error(try(log(1), silent = TRUE)))
})
test_that("scalar type predicates return a logical scalar", {
  expect_true(is_scalar_list(list(1)))
  expect_false(is_scalar_list(list()))
  expect_false(is_scalar_list(list(1, 2)))

  expect_true(is_scalar_atomic(numeric(1)))
  expect_false(is_scalar_atomic(numeric(0)))
  expect_false(is_scalar_atomic(numeric(2)))

  expect_true(is_scalar_vector(numeric(1)))
  expect_false(is_scalar_vector(numeric(0)))
  expect_false(is_scalar_vector(numeric(2)))

  expect_true(is_scalar_numeric(numeric(1)))
  expect_false(is_scalar_numeric(numeric(0)))
  expect_false(is_scalar_numeric(numeric(2)))

  expect_true(is_scalar_integer(integer(1)))
  expect_false(is_scalar_integer(integer(0)))
  expect_false(is_scalar_integer(integer(2)))

  expect_true(is_scalar_double(double(1)))
  expect_false(is_scalar_double(double(0)))
  expect_false(is_scalar_double(double(2)))

  expect_true(is_scalar_character(character(1)))
  expect_false(is_scalar_character(character(0)))
  expect_false(is_scalar_character(character(2)))

  expect_true(is_scalar_logical(logical(1)))
  expect_false(is_scalar_logical(logical(0)))
  expect_false(is_scalar_logical(logical(2)))
})
test_that("numeric predicates return a logical vector", {
  expect_type(is_odd(c(1, 3, 5, 7, 9)), "logical")
  expect_false(is_odd(2))

  expect_type(is_positive(c(0, 1, 2, 3, 4)), "logical")
  expect_true(all(is_positive(c(0, 1, 2, 3, 4), strict = FALSE)))
  expect_false(all(is_positive(c(0, 1, 2, 3, 4), strict = TRUE)))
  expect_error(is_positive(LETTERS))

  expect_type(is_whole(c(0, 1, 2, 3, 4)), "logical")
  expect_true(all(is_whole(c(0, 1, 2, 3, 4))))
  expect_true(is_whole(0.1, tolerance = 0.1))
  expect_false(is_whole(0.1))
  expect_error(is_whole(LETTERS))

  expect_type(is_binary(c(1, 0, 1, 0, 1)), "logical")
  expect_true(all(is_binary(c(1, 0, 1, 0, 1))))
  expect_false(is_binary(2))
  expect_error(is_binary(LETTERS))

  expect_true(is_equal(c(1, 1, 1, 1, 1)))
  expect_true(is_equal(c(1, 1.1, 1.2, 1.3, 1.4), tolerance = 0.5))
  expect_true(is_equal(c(1, 1, 1, NA, 1), na.rm = TRUE))
  expect_false(is_equal(c(1, 1, 1, NA, 1), na.rm = FALSE))
  expect_false(is_equal(c(1, 2, 1, 1, 1)))
  expect_error(is_equal(LETTERS))

  expect_true(is_increasing(c(1, 2, 3, 4, 5)))
  expect_true(is_increasing(c(1, 1, 1, 1, 1)))
  expect_true(is_increasing(c(1, 2, 3, NA, 5), na.rm = TRUE))
  expect_false(is_increasing(c(1, 2, 3, NA, 5), na.rm = FALSE))
  expect_false(is_increasing(c(5, 4, 3, 2, 1)))
  expect_error(is_increasing(LETTERS))

  expect_true(is_decreasing(c(5, 4, 3, 2, 1)))
  expect_true(is_decreasing(c(1, 1, 1, 1, 1)))
  expect_true(is_decreasing(c(5, 4, 3, NA, 1), na.rm = TRUE))
  expect_false(is_decreasing(c(5, 4, 3, NA, 1), na.rm = FALSE))
  expect_false(is_decreasing(c(1, 2, 3, 4, 5)))
  expect_error(is_decreasing(LETTERS))

  expect_true(is_overlapping(c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 5, 6, 7)))
  expect_true(is_overlapping(c(1, 2, 3, 4, 5, 6, 7), c(1, 2, 3, 4, 5)))
  expect_false(is_overlapping(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10)))
  expect_false(is_overlapping(c(1, 2, 3, 4, 5), c(7, 8, 9, 10, 11)))
  expect_error(is_overlapping(1:5, LETTERS))
})
test_that("matrix predicates return a logical scalar", {
  expect_true(is_square(matrix(nrow = 3, ncol = 3)))
  expect_false(is_square(matrix(nrow = 1, ncol = 3)))
  expect_false(is_square(c(1, 2, 3)))

  mtx1 <- matrix(sample(seq_len(25), size = 25), nrow = 5, ncol = 5)
  mtx2 <- t(mtx1)
  mtx2[lower.tri(mtx2)] <- mtx1[lower.tri(mtx1)]
  expect_true(is_symmetric(mtx2))
  expect_false(is_symmetric(mtx1))
  expect_false(is_symmetric(c(1, 2, 3)))
})
