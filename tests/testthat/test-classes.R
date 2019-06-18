context("Classes")
options("verbose" = TRUE)

# Seriation ====================================================================
test_that("Initialize a PermutationOrder object", {
  expect_s4_class(.PermutationOrder(), "PermutationOrder")
  expect_message(PermutationOrder())

  expect_s4_class(PermutationOrder(rows = 1:10, columns = 1:10, method = "X"),
                  "PermutationOrder")

  expect_error(PermutationOrder(id = NA_character_),
               "`id` must be a character string.")
  expect_error(PermutationOrder(id = LETTERS),
               "`id` must be a character string.")
  expect_error(PermutationOrder(id = "a"),
               "must be a 36 characters long string")
  expect_error(PermutationOrder(columns = 1:10, rows = 0:10),
               "`rows` must contain positive numbers.")
  expect_error(PermutationOrder(columns = 1:10, rows = -5:-1),
               "`rows` must contain positive numbers.")
  expect_error(PermutationOrder(columns = 1:10, rows = c(1, 2, NA)),
               "must not contain missing values")
  expect_error(PermutationOrder(rows = 1:10, columns = 0:10),
               "`columns` must contain positive numbers.")
  expect_error(PermutationOrder(rows = 1:10, columns = -5:-1),
               "`columns` must contain positive numbers.")
  expect_error(PermutationOrder(rows = 1:10, columns = c(1, 2, NA)),
               "must not contain missing values")
  expect_error(PermutationOrder(method = NA_character_),
               "must not contain missing values", class = "error")
  expect_error(PermutationOrder(method = LETTERS),
               "`method` must be a scalar", class = "error")
})
test_that("Initialize a BootCA object", {
  expect_s4_class(.BootCA(), "BootCA")
  expect_s4_class(BootCA(), "BootCA")

  expect_s4_class(
    BootCA(
      rows = list(id = LETTERS, x = 1:26, y = 1:26),
      columns = list(id = letters, x = 1:26, y = 1:26),
      lengths = list(1:26, 1:26),
      cutoff = c(3, 2),
      keep = list(1:3, 1:3)
    ),
    "BootCA"
  )

  expect_error(.BootCA(rows = list(1:5)))
  expect_error(.BootCA(rows = list(id = 1:26, m = 1:26, n = 1:26)))
  expect_error(.BootCA(rows = list(id = 1:26, x = LETTERS, y = 1:26)))

  expect_error(.BootCA(columns = list(1:5)))
  expect_error(.BootCA(columns = list(id = 1:26, m = 1:26, n = 1:26)))
  expect_error(.BootCA(columns = list(id = 1:26, x = LETTERS, y = 1:26)))

  expect_error(.BootCA(lengths = list(id = 1:26, m = 1:26)))
  expect_error(.BootCA(lengths = list(id = 1:26, d = LETTERS)))

  expect_error(.BootCA(cutoff = 1:3),
               "`cutoff` must be of length 2; not 3.")
})
# Dating =======================================================================
test_that("Initialize an empty DateModel object", {
  expect_s4_class(.DateModel(), "DateModel")
  expect_message(DateModel())
})
test_that("Initialize a DateModel object", {
  mtx <- cbind(a = 1:5, b = 1:5)
  mtx[1, 1] <- NA

  expect_error(DateModel(id = "X"),
               "`id` must be a 36 characters long string; not 1.")
  expect_error(DateModel(level = 1:2),
               "`level` must be a scalar")
  expect_error(DateModel(level = NA_real_),
               "`level` must not contain missing values")
  expect_error(DateModel(level = Inf),
               "`level` must not contain infinite values")
  expect_error(DateModel(rows = mtx),
               "`rows` must not contain missing values")
  expect_error(DateModel(rows = mtx),
               "`rows` must have the following column names")
  expect_error(DateModel(columns = mtx),
               "`columns` must not contain missing values")
  expect_error(DateModel(columns = mtx),
               "`columns` must have the following column names")
})
