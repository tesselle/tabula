context("Predicates")

test_that("utility predicates return a logical scalar", {
  expect_true(isEmpty(numeric(0)))
  expect_false(isEmpty(numeric(1)))

  A <- LETTERS
  B <- c("D", "E", "F")
  b <- c("d", "e", "f")
  expect_true(isSubset(B, A))
  expect_true(isSubset(LETTERS, LETTERS))
  expect_false(isSubset(A, B))
  expect_false(isSubset(A, b))
})
test_that("type predicates return a logical scalar", {
  expect_true(isList(list()))
  expect_false(isList(numeric(1)))

  expect_true(isAtomic(numeric(1)))
  expect_false(isAtomic(list()))

  expect_true(isVector(numeric(1)))
  expect_true(isVector(list()))
  expect_false(isVector(expression()))

  expect_true(isNumeric(1L))
  expect_true(isNumeric(1.5))
  expect_false(isNumeric("A"))

  expect_true(isInteger(1L))
  expect_false(isInteger(1.5))

  expect_true(isDouble(1.5))
  expect_false(isDouble(1L))

  expect_true(isCharacter("A"))
  expect_false(isCharacter(1))

  expect_true(isLogical(TRUE))
  expect_false(isLogical(1))
})
test_that("scalar type predicates return a logical scalar", {
  expect_true(isScalarList(list(1)))
  expect_false(isScalarList(list()))
  expect_false(isScalarList(list(1, 2)))

  expect_true(isScalarAtomic(numeric(1)))
  expect_false(isScalarAtomic(numeric(0)))
  expect_false(isScalarAtomic(numeric(2)))

  expect_true(isScalarVector(numeric(1)))
  expect_false(isScalarVector(numeric(0)))
  expect_false(isScalarVector(numeric(2)))

  expect_true(isScalarNumeric(numeric(1)))
  expect_false(isScalarNumeric(numeric(0)))
  expect_false(isScalarNumeric(numeric(2)))

  expect_true(isScalarInteger(integer(1)))
  expect_false(isScalarInteger(integer(0)))
  expect_false(isScalarInteger(integer(2)))

  expect_true(isScalarDouble(double(1)))
  expect_false(isScalarDouble(double(0)))
  expect_false(isScalarDouble(double(2)))

  expect_true(isScalarCharacter(character(1)))
  expect_false(isScalarCharacter(character(0)))
  expect_false(isScalarCharacter(character(2)))

  expect_true(isScalarLogical(logical(1)))
  expect_false(isScalarLogical(logical(0)))
  expect_false(isScalarLogical(logical(2)))
})
test_that("numeric predicates return a logical vector", {
  expect_type(isOdd(c(1, 3, 5, 7, 9)), "logical")
  expect_false(isOdd(2))

  expect_type(isPositive(c(0, 1, 2, 3, 4)), "logical")
  expect_true(all(isPositive(c(0, 1, 2, 3, 4), strict = FALSE)))
  expect_false(all(isPositive(c(0, 1, 2, 3, 4), strict = TRUE)))
  expect_error(isPositive(LETTERS))

  expect_type(isWholeNumber(c(0, 1, 2, 3, 4)), "logical")
  expect_true(all(isWholeNumber(c(0, 1, 2, 3, 4))))
  expect_true(isWholeNumber(0.1, tolerance = 0.1))
  expect_false(isWholeNumber(0.1))
  expect_error(isWholeNumber(LETTERS))

  expect_type(isBinary(c(1, 0, 1, 0, 1)), "logical")
  expect_true(all(isBinary(c(1, 0, 1, 0, 1))))
  expect_false(isBinary(2))
  expect_error(isBinary(LETTERS))

  expect_true(isEqual(c(1, 1, 1, 1, 1)))
  expect_true(isEqual(c(1, 1.1, 1.2, 1.3, 1.4), tolerance = 0.5))
  expect_true(isEqual(c(1, 1, 1, NA, 1), na.rm = TRUE))
  expect_false(isEqual(c(1, 1, 1, NA, 1), na.rm = FALSE))
  expect_false(isEqual(c(1, 2, 1, 1, 1)))
  expect_error(isEqual(LETTERS))

  expect_true(isIncreasing(c(1, 2, 3, 4, 5)))
  expect_true(isIncreasing(c(1, 1, 1, 1, 1)))
  expect_true(isIncreasing(c(1, 2, 3, NA, 5), na.rm = TRUE))
  expect_false(isIncreasing(c(1, 2, 3, NA, 5), na.rm = FALSE))
  expect_false(isIncreasing(c(5, 4, 3, 2, 1)))
  expect_error(isIncreasing(LETTERS))

  expect_true(isDecreasing(c(5, 4, 3, 2, 1)))
  expect_true(isDecreasing(c(1, 1, 1, 1, 1)))
  expect_true(isDecreasing(c(5, 4, 3, NA, 1), na.rm = TRUE))
  expect_false(isDecreasing(c(5, 4, 3, NA, 1), na.rm = FALSE))
  expect_false(isDecreasing(c(1, 2, 3, 4, 5)))
  expect_error(isDecreasing(LETTERS))

  expect_true(isOverlapping(c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 5, 6, 7)))
  expect_true(isOverlapping(c(1, 2, 3, 4, 5, 6, 7), c(1, 2, 3, 4, 5)))
  expect_false(isOverlapping(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10)))
  expect_false(isOverlapping(c(1, 2, 3, 4, 5), c(7, 8, 9, 10, 11)))
  expect_error(isOverlapping(1:5, LETTERS))
})
test_that("matrix predicates return a logical scalar", {
  expect_true(isSquare(matrix(nrow = 3, ncol = 3)))
  expect_false(isSquare(matrix(nrow = 1, ncol = 3)))
  expect_false(isSquare(c(1, 2, 3)))

  mtx1 <- matrix(sample(seq_len(25), size = 25), nrow = 5, ncol = 5)
  mtx2 <- t(mtx1)
  mtx2[lower.tri(mtx2)] <- mtx1[lower.tri(mtx1)]
  expect_true(isSymmetric(mtx2))
  expect_false(isSymmetric(mtx1))
  expect_false(isSymmetric(c(1, 2, 3)))
})
