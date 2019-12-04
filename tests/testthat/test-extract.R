context("Extract")

test_that("BootCA", {
  boot_ca <- new("BootCA")

  expect_s3_class(boot_ca[["rows"]], "data.frame")
  expect_s3_class(boot_ca[["columns"]], "data.frame")
  expect_type(boot_ca[["lengths"]], "list")
  expect_type(boot_ca[["cutoff"]], "double")
  expect_type(boot_ca[["keep"]], "list")
  expect_error(boot_ca[["X"]])

  expect_s3_class(boot_ca["rows", , drop = FALSE], "data.frame")
  expect_s3_class(boot_ca["rows", NULL , drop = FALSE], "data.frame")
  expect_s3_class(boot_ca["rows", 1 , drop = FALSE], "data.frame")
  expect_s3_class(boot_ca["rows", "a" , drop = FALSE], "data.frame")
  expect_s3_class(boot_ca["columns",  , drop = FALSE], "data.frame")
  expect_error(boot_ca["X", ])
})
test_that("DateModel", {
  mtx <- matrix(data = 1, nrow = 3, ncol = 3,
                dimnames = list(c("A", "B", "C"), NULL))
  date_model <- new("DateModel", counts = mtx)

  expect_type(date_model[["counts"]], "double")
  expect_type(date_model[["level"]], "double")
  expect_s3_class(date_model[["model"]], "lm")
  expect_type(date_model[["rows"]], "double")
  expect_type(date_model[["columns"]], "double")
  expect_type(date_model[["accumulation"]], "double")
  expect_error(date_model[["X"]])

  expect_type(date_model["counts", , drop = FALSE], "double")
  expect_type(date_model["counts", NULL, drop = FALSE], "double")
  expect_type(date_model["counts", 1, drop = TRUE], "double")
  expect_type(date_model["counts", 1:2], "double")
  expect_type(date_model["counts", "A"], "double")
  expect_type(date_model["rows", ], "double")
  expect_type(date_model["rows", NULL], "double")
  expect_type(date_model["rows", 1], "double")
  expect_type(date_model["columns", ], "double")
  expect_type(date_model["accumulation", ], "double")
  expect_error(date_model["X", ])
})
test_that("PermutationOrder", {
  perm_order <- new("PermutationOrder")

  expect_type(perm_order[["rows"]], "integer")
  expect_type(perm_order[["columns"]], "integer")
  expect_type(perm_order[["method"]], "character")
})
