context("Classes")

# Seriation ====================================================================
test_that("Initialize a PermutationOrder object", {
  expect_s4_class(.PermutationOrder(), "PermutationOrder")

  cnd <- catch_conditions(.PermutationOrder(id = NA_character_))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be an UUID", cnd[[1]]$message))

  cnd <- catch_conditions(.PermutationOrder(id = "X"))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be an UUID", cnd[[1]]$message))

  cnd <- catch_conditions(.PermutationOrder(columns = 1:10, rows = 0:10))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must contain positive numbers", cnd[[1]]$message))

  cnd <- catch_conditions(.PermutationOrder(columns = 1:10, rows = -5:-1))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must contain positive numbers", cnd[[1]]$message))

  cnd <- catch_conditions(.PermutationOrder(rows = 1:10, columns = 0:10))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must contain positive numbers", cnd[[1]]$message))

  cnd <- catch_conditions(.PermutationOrder(rows = 1:10, columns = -5:-1))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must contain positive numbers", cnd[[1]]$message))

  cnd <- catch_conditions(.PermutationOrder(method = NA_character_))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))

  cnd <- catch_conditions(.PermutationOrder(method = LETTERS))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be a scalar", cnd[[1]]$message))

})
test_that("Initialize a BootCA object", {
  expect_s4_class(.BootCA(), "BootCA")

  cnd <- catch_conditions(.BootCA(rows = list(1:5)))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be of length", cnd[[1]]$message))
  expect_true(grepl("must have the following names", cnd[[1]]$message))

  cnd <- catch_conditions(.BootCA(rows = list(id = 1:26, m = 1:26, n = 1:26)))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must have the following names", cnd[[1]]$message))

  cnd <- catch_conditions(.BootCA(rows = list(id = 1:26, x = LETTERS, y = 1:26)))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be numeric; not character", cnd[[1]]$message))

  cnd <- catch_conditions(.BootCA(columns = list(1:5)))
  expect_true(grepl("must be of length", cnd[[1]]$message))
  expect_true(grepl("must have the following names", cnd[[1]]$message))

  cnd <- catch_conditions(.BootCA(columns = list(id = 1:26, m = 1:26, n = 1:26)))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must have the following names", cnd[[1]]$message))

  cnd <- catch_conditions(.BootCA(columns = list(id = 1:26, x = LETTERS, y = 1:26)))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be numeric; not character", cnd[[1]]$message))

  # cnd <- catch_conditions(.BootCA(lengths = list(id = 1:26, m = 1:26)))
  # expect_s3_class(cnd[[1]], "arkhe_error_class")
  # expect_true(grepl("", cnd[[1]]$message))

  cnd <- catch_conditions(.BootCA(lengths = list(id = 1:26, d = LETTERS)))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be numeric; not character", cnd[[1]]$message))

  cnd <- catch_conditions(.BootCA(cutoff = 1:3))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be of length 2; not 3", cnd[[1]]$message))
})
# Dating =======================================================================
test_that("Initialize a DateModel object", {
  expect_s4_class(.DateModel(), "DateModel")
  expect_s4_class(.DateModel(), "DateModel")

  cnd <- catch_conditions(.DateModel(id = "X"))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be an UUID", cnd[[1]]$message))

  cnd <- catch_conditions(.DateModel(level = 1:2))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must be a scalar", cnd[[1]]$message))

  cnd <- catch_conditions(.DateModel(level = NA_real_))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))

  cnd <- catch_conditions(.DateModel(level = Inf))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must not contain infinite values", cnd[[1]]$message))

  mtx <- cbind(a = 1:5, b = 1:5)
  mtx[1, 1] <- NA

  cnd <- catch_conditions(.DateModel(rows = mtx))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))

  cnd <- catch_conditions(.DateModel(rows = mtx))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must have the following column names", cnd[[1]]$message))

  cnd <- catch_conditions(.DateModel(columns = mtx))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))

  cnd <- catch_conditions(.DateModel(columns = mtx))
  expect_s3_class(cnd[[1]], "arkhe_error_class")
  expect_true(grepl("must have the following column names", cnd[[1]]$message))
})
