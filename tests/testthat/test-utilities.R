context("Utilities")

test_that("Binary numbers", {
  expect_true(isBinary(c(1,0,1,0,1)))
  expect_false(isBinary(1:5))
  expect_error(isBinary(LETTERS))
})
test_that("Equality within a vector", {
  expect_true(isEqual(c(1,1,1,1,1)))
  expect_true(isEqual(c(1,2,1,1,1), tolerance = 1))
  expect_true(isEqual(c(1,NA,1,1,1), na.rm = TRUE))
  expect_equal(isEqual(c(1,NA,1,1,1), na.rm = FALSE), NA)
  expect_false(isEqual(1:5))
  expect_error(isEqual(LETTERS))
})
test_that("Trends", {
  expect_true(isIncreasing(1:5))
  expect_true(isIncreasing(c(1:5, NA), na.rm = TRUE))
  expect_equal(isIncreasing(c(1:5, NA), na.rm = FALSE), NA)
  expect_true(isIncreasing(c(1,1,1,1,1)))
  expect_false(isIncreasing(5:1))
  expect_error(isIncreasing(LETTERS))

  expect_true(isDecreasing(5:1))
  expect_true(isDecreasing(c(5:1, NA), na.rm = TRUE))
  expect_equal(isDecreasing(c(5:1, NA), na.rm = FALSE), NA)
  expect_true(isDecreasing(c(1,1,1,1,1)))
  expect_false(isDecreasing(1:5))
  expect_error(isDecreasing(LETTERS))
})
test_that("Overlap", {
  expect_true(isOverlapping(c(5:10), c(4:11)))
  expect_true(isOverlapping(c(5:10), c(1:5)))
  expect_true(isOverlapping(c(5:10), c(10:15)))
  expect_false(isOverlapping(c(5:10), c(1:4)))
  expect_false(isOverlapping(c(5:10), c(11:15)))
  expect_error(isOverlapping(1:5, LETTERS))
})
test_that("Positive numbers", {
  expect_true(isPositive(1:5, strict = TRUE))
  expect_true(isPositive(0:5, strict = FALSE))
  expect_true(isPositive(c(1:5, NA), na.rm = TRUE))
  expect_true(isPositive(c(1:5, NaN), na.rm = TRUE))
  expect_equal(isPositive(c(1:5, NA), na.rm = FALSE), NA)
  expect_equal(isPositive(c(1:5, NaN), na.rm = FALSE), NA)
  expect_false(isPositive(1:-5, strict = FALSE))
  expect_false(isPositive(0:5, strict = TRUE))
  expect_error(isPositive(LETTERS))
})
test_that("Square matrix", {
  s <- matrix(nrow = 3, ncol = 3)
  expect_true(isSquare(s))
  expect_false(isSquare(as.data.frame(s)))
  expect_false(isSquare(cbind(s, s)))
})
test_that("Contain", {
  A <- LETTERS
  B <- c("D", "E", "F")
  b <- c("d", "e", "f")
  expect_true(isSubset(B, A))
  expect_true(isSubset(LETTERS, LETTERS))
  expect_false(isSubset(A, B))
  expect_false(isSubset(A, b))
})
test_that("Symmetric matrix", {
  s <- matrix(nrow = 3, ncol = 3)
  s[upper.tri(s)] <- 1
  s[lower.tri(s)] <- 1
  expect_true(isSymmetric(s))
  expect_false(isSymmetric(as.data.frame(s)))
  expect_false(isSymmetric(cbind(s, s)))

  s[lower.tri(s)] <- 2
  expect_false(isSymmetric(s))
})
test_that("Integer numbers", {
  expect_true(any(isWholeNumber(1:5)))
  expect_true(any(isWholeNumber(c(1:5, 6.2), tolerance = 0.2)))
  expect_false(sum(isWholeNumber(seq(1, 2, 0.1))) == 5)
  expect_error(isWholeNumber(LETTERS))
})
