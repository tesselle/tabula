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
test_that("SpaceTime", {
  space_time <- new("SpaceTime")
  expect_type(space_time[["dates"]], "list")
  expect_type(space_time[["coordinates"]], "list")
})
test_that("AbundanceMatrix", {
  options("verbose" = TRUE)
  A1 <- CountMatrix(data = sample(0:10, 100, TRUE),
                    nrow = 10, ncol = 10, byrow = TRUE)
  # ID
  expect_type(get_id(A1), "character")
})
test_that("AbundanceMatrix - Dates", {
  options("verbose" = TRUE)
  A1 <- CountMatrix(data = sample(0:10, 100, TRUE),
                    nrow = 10, ncol = 10, byrow = TRUE)

  # Time coordinates
  dates <- list(value = seq_len(10), error = rep(0, 10))
  roman <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X")
  num <- seq_len(10)

  expect_message(set_dates(A1) <- roman)
  expect_equal(get_dates(A1), dates)
  expect_message(set_dates(A1) <- num)
  expect_equal(get_dates(A1), dates)
  expect_message(set_dates(A1) <- dates)
  expect_equal(get_dates(A1), dates)
  expect_message(set_dates(A1) <- as.data.frame(dates))
  expect_equal(get_dates(A1), dates)
  expect_message(set_dates(A1) <- as.data.frame(unname(dates)))
  expect_equal(get_dates(A1), dates)

  expect_error(set_dates(A1) <- as.data.frame(dates[-2]),
               "should have at least 2 columns")
  expect_error(set_dates(A1) <- dates[1], "does not have components")
  expect_error(set_dates(A1) <- NA,
               "a list, a matrix or a data frame is expected")
  expect_error(set_dates(A1) <- "X", "Cannot interpret")
  expect_warning(set_dates(A1) <- rep("A", 10))
})
test_that("AbundanceMatrix - Coordinates", {
  options("verbose" = TRUE)
  A1 <- CountMatrix(data = sample(0:10, 100, TRUE),
                    nrow = 10, ncol = 10, byrow = TRUE)

  # Geographic coordinates
  set_epsg(A1) <- 12345
  expect_equal(get_epsg(A1), 12345)
  expect_equal(attr(get_coordinates(A1), "epsg"), 12345)
  expect_error(set_epsg(A1) <- "X")

  coords <- list(X = sample(0:10, 10, TRUE),
                 Y = sample(0:10, 10, TRUE),
                 Z = sample(0:10, 10, TRUE))
  expect_message(set_coordinates(A1) <- coords[-3])
  expect_equal(get_coordinates(A1)[-3], coords[-3])
  expect_equal(get_coordinates(A1)[[3]], rep(NA_real_, 10))
  expect_silent(set_coordinates(A1) <- coords)
  expect_equal(get_coordinates(A1)[seq_len(3)], coords)
  expect_silent(set_coordinates(A1) <- as.data.frame(coords))
  expect_equal(get_coordinates(A1)[seq_len(3)], coords)
  expect_silent(set_coordinates(A1) <- as.data.frame(unname(coords)))
  expect_equal(get_coordinates(A1)[seq_len(3)], coords)
  expect_message(set_coordinates(A1) <- as.data.frame(coords[-3]))
  expect_equal(get_coordinates(A1)[-3], coords[-3])

  expect_error(set_coordinates(A1) <- as.data.frame(coords[-c(1, 2)]),
               "should have at least 2 columns")
  expect_error(set_coordinates(A1) <- coords[1], "does not have components")
  expect_error(set_coordinates(A1) <- rep("A", 10),
               "A list, a matrix or a data frame is expected")
})
test_that("NumericMatrix", {
  mtx_count <- matrix(sample(1:100, 100, TRUE), ncol = 10,
                      dimnames = list(LETTERS[1:10], LETTERS[26:17]))
  freq <- .FrequencyMatrix(mtx_count / rowSums(mtx_count),
                           totals = rowSums(mtx_count))

  expect_equal(get_totals(freq), rowSums(mtx_count))
  set_totals(freq) <- seq_len(10)
  expect_equal(get_totals(freq), seq_len(10))
})
test_that("PermutationOrder", {
  perm_order <- new("PermutationOrder")

  expect_type(perm_order[["rows"]], "integer")
  expect_type(perm_order[["columns"]], "integer")
  expect_type(perm_order[["method"]], "character")
})
