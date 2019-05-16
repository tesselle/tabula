context("Classes")
options("verbose" = TRUE)

# Seriation ====================================================================
test_that("Initialize a PermutationOrder object", {
  expect_s4_class(.PermutationOrder(), "PermutationOrder")
  expect_message(PermutationOrder())

  expect_s4_class(PermutationOrder(rows = 1:10, columns = 1:10, method = "X"),
                  "PermutationOrder")

  expect_error(PermutationOrder(id = NA_character_),
               "a missing value was detected")
  expect_error(PermutationOrder(id = LETTERS),
               "should be of length 1, not 26")
  expect_error(PermutationOrder(id = "a"),
               "should be 36 characters long string, not 1")
  expect_error(PermutationOrder(columns = 1:10, rows = 0:10),
               "positive values are expected")
  expect_error(PermutationOrder(columns = 1:10, rows = -5:-1),
               "positive values are expected")
  expect_error(PermutationOrder(columns = 1:10, rows = c(1, 2, NA)),
               "a missing value was detected")
  expect_error(PermutationOrder(rows = 1:10, columns = 0:10),
               "positive values are expected")
  expect_error(PermutationOrder(rows = 1:10, columns = -5:-1),
               "positive values are expected")
  expect_error(PermutationOrder(rows = 1:10, columns = c(1, 2, NA)),
               "a missing value was detected")
  expect_error(PermutationOrder(method = NA_character_),
               "a missing value was detected")
  expect_error(PermutationOrder(method = LETTERS),
               "should be of length 1, not 26")
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

  expect_error(new("BootCA", rows = list(1:5)))
  expect_error(new("BootCA", rows = list(id = 1:26, m = 1:26, n = 1:26)))
  expect_error(new("BootCA", rows = list(id = 1:26, x = LETTERS, y = 1:26)))

  expect_error(new("BootCA", columns = list(1:5)))
  expect_error(new("BootCA", columns = list(id = 1:26, m = 1:26, n = 1:26)))
  expect_error(new("BootCA", columns = list(id = 1:26, x = LETTERS, y = 1:26)))

  expect_error(new("BootCA", lengths = LETTERS))
  expect_error(new("BootCA", lengths = list(id = 1:26, m = 1:26)))
  expect_error(new("BootCA", lengths = list(id = 1:26, d = LETTERS)))

  expect_error(new("BootCA", cutoff = 1:3))
  expect_error(new("BootCA", keep = LETTERS))
})
# Dating =======================================================================
test_that("Initialize an empty DateModel object", {
  expect_s4_class(new("DateModel"), "DateModel")
})
test_that("Initialize a DateModel object", {
  mtx <- matrix(data = 1, ncol = 26, nrow = 26)
  df1 <- data.frame(id = LETTERS, date = 1:26)
  df2 <- data.frame(id = LETTERS, estimation = 1:26, earliest = 1:26,
                    latest = 1:26, error = 1:26)
  df3 <- data.frame(id = LETTERS, estimation = 1:26)
  df4 <- data.frame(id = LETTERS, estimation = 1:26, earliest = 1:26,
                    latest = 1:26, error = 1:26, bias = 1:26)
  df5 <- data.frame(id = LETTERS, min = 1:26, Q05 = 1:26, mean = 1:26,
                    Q95 = 1:26, max = 1:26)
  expect_s4_class(
    new("DateModel",
        level = 0.5,
        model = stats::lm(0 ~ 0),
        residual = 10,
        counts = mtx,
        dates = df1,
        rows = df2,
        columns = df2,
        accumulation = df3,
        jackknife = df4,
        bootstrap = df5),
    "DateModel"
  )

  expect_error(new("DateModel", level = 10))
  expect_error(new("DateModel", level = NA_real_))
  expect_error(new("DateModel", level = 1:5))
  expect_error(new("DateModel", residual = NA_real_))
  expect_error(new("DateModel", residual = 1:5))
  expect_error(new("DateModel", rows = data.frame(a = 1:5, b = 1:5)))
  expect_error(new("DateModel", columns = data.frame(a = 1:5, b = 1:5)))
  expect_error(new("DateModel", jackknife = data.frame(a = 1:5, b = 1:5)))
  expect_error(new("DateModel", bootstrap = data.frame(a = 1:5, b = 1:5)))
  expect_error(new("DateModel",
                   rows = data.frame(a = 1:5, b = 1:5, c = 1:5, d = 1:5, e = 1:5),
                   jackknife = data.frame(a = 1:2, b = 1:2, c = 1:2, d = 1:2, e = 1:2)))
  expect_error(new("DateModel",
                   rows = data.frame(a = 1:5, b = 1:5, c = 1:5, d = 1:5, e = 1:5),
                   bootstrap = data.frame(a = 1:2, b = 1:2, c = 1:2, d = 1:2, e = 1:2)))
  expect_error(new("DateModel",
                   jackknife = data.frame(a = 1:5, b = 1:5, c = 1:5, d = 1:5, e = 1:5),
                   bootstrap = data.frame(a = 1:2, b = 1:2, c = 1:2, d = 1:2, e = 1:2)))
})
