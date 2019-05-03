context("Classes initialization")
options("verbose" = TRUE)

# Seriation ====================================================================
test_that("Initialize an empty PermutationOrder object", {
  expect_s4_class(new("PermutationOrder"), "PermutationOrder")
})
test_that("Initialize a PermutationOrder object", {
  expect_s4_class(
    new("PermutationOrder", rows = 1:10, columns = 1:10, method = "X"),
    "PermutationOrder"
  )

  expect_error(new("PermutationOrder", rows = 1:10))
  expect_error(new("PermutationOrder", columns = 1:10, rows = LETTERS))
  expect_error(new("PermutationOrder", columns = 1:10, rows = 0:10))
  expect_error(new("PermutationOrder", columns = 1:10, rows = -5:-1))
  expect_error(new("PermutationOrder", columns = 1:10, rows = c(1,2,NA)))

  expect_error(new("PermutationOrder", columns = 1:10))
  expect_error(new("PermutationOrder", rows = 1:10, columns = LETTERS))
  expect_error(new("PermutationOrder", rows = 1:10, columns = 0:10))
  expect_error(new("PermutationOrder", rows = 1:10, columns = -5:-1))
  expect_error(new("PermutationOrder", rows = 1:10, columns = c(1,2,NA)))

  expect_error(new("PermutationOrder", rows = 1:10, columns = 1:10))
  expect_error(new("PermutationOrder", method = NA))
  expect_error(new("PermutationOrder", method = 1))
  expect_error(new("PermutationOrder", method = LETTERS))
})
test_that("Initialize an empty BootCA object", {
  expect_s4_class(new("BootCA"), "BootCA")
})
test_that("Initialize a BootCA object", {
  expect_s4_class(
    new("BootCA",
        rows = data.frame(id = LETTERS, x = 1:26, y = 1:26),
        columns = data.frame(id = LETTERS, x = 1:26, y = 1:26),
        length = data.frame(id = LETTERS, d = 1:26),
        cutoff = 3,
        keep = 1:3),
    "BootCA"
  )

  expect_error(new("BootCA", rows = data.frame(1:5)))
  expect_error(new("BootCA", rows = data.frame(id = 1:26, m = 1:26, n = 1:26)))
  expect_error(new("BootCA", rows = data.frame(id = 1:26, x = LETTERS, y = 1:26)))

  expect_error(new("BootCA", columns = data.frame(1:5)))
  expect_error(new("BootCA", columns = data.frame(id = 1:26, m = 1:26, n = 1:26)))
  expect_error(new("BootCA", columns = data.frame(id = 1:26, x = LETTERS, y = 1:26)))

  expect_error(new("BootCA", lengths = LETTERS))
  expect_error(new("BootCA", lengths = data.frame(id = 1:26, m = 1:26)))
  expect_error(new("BootCA", lengths = data.frame(id = 1:26, d = LETTERS)))

  expect_error(new("BootCA", cutoff = 1:2))
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
