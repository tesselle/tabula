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
test_that("Row names to column", {
  mtx <- matrix(data = seq_len(260), nrow = 26, ncol = 2,
                dimnames = list(LETTERS, NULL))

  mtx2 <- rownames_to_column(mtx, factor = TRUE, id = "id")
  expect_identical(dim(mtx2), c(26L, 3L))
  expect_identical(colnames(mtx2), c("id", "col1", "col2"))
  expect_identical(unname(mtx2[, 1]), as.factor(LETTERS))
  expect_s3_class(mtx2, "data.frame")

  df <- as.data.frame(mtx)
  df2 <- rownames_to_column(df, factor = FALSE)
  expect_identical(dim(df2), c(26L, 3L))
  expect_identical(colnames(df2), c("id", "V1", "V2"))
  expect_identical(unname(df2[, 1]), LETTERS)
  expect_s3_class(df2, "data.frame")

  expect_error(rownames_to_column(LETTERS, factor = TRUE, id = NULL))
})
# test_that("Rolling window", {
#   mtx <- matrix(data = seq_len(100), nrow = 20)
#   roll_index <- roll(mtx, window = 3)
# })
