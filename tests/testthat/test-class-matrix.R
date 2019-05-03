context("Matrix classes initialization")
options("verbose" = TRUE)

# Create a count matrix with dimnames
mtx_count <- matrix(sample(1:100, 100, TRUE), ncol = 10,
                    dimnames = list(LETTERS[1:10], LETTERS[26:17]))
# Create a relative frequency matrix
mtx_freq <- mtx_count / rowSums(mtx_count)
# Create an logical matrix
vec_incid <- as.logical(sample(0:1, 100, TRUE, prob = c(0.25, 0.75)))
mtx_incid <- matrix(vec_incid, ncol = 10)

# Create a character matrix
mtx_character <- matrix(sample(LETTERS, 100, TRUE), ncol = 10)
# Create a numeric matrix with NA, Inf and NaN values
mtx_num_na <- mtx_num_inf <- mtx_num_nan <- mtx_count
mtx_num_na[sample(1:100, 10, FALSE)] <- NA
mtx_num_inf[sample(1:100, 10, FALSE)] <- Inf
mtx_num_nan[sample(1:100, 10, FALSE)] <- NaN

# Create a logical matrix with NA values
mtx_logic_na <- mtx_incid
mtx_logic_na[sample(1:100, 10, FALSE)] <- NA

# Space-Time ===================================================================
test_that("Initialize a SpaceTime instance", {
  # Empty instence
  expect_s4_class(new("SpaceTime"), "SpaceTime")

  dates <- list(value = 1:10, error = 1:10)
  coord <- list(x = 1:10, y = 1:10, z = 1:10)
  expect_s4_class(
    new("SpaceTime", dates = dates, coordinates = coord, epsg = 4326),
    "SpaceTime"
  )
  expect_message(
    new("SpaceTime", dates = dates, coordinates = coord, epsg = 4326),
    "SpaceTime"
  )

  # Try Inf
  dates[[1]][1] <- Inf
  coord[[1]][1] <- Inf
  expect_error(new("SpaceTime", dates = dates, coord = coord),
               "Infinite values are not allowed")
  # Try NaN
  dates[[1]][1] <- NaN
  coord[[1]][1] <- NaN
  expect_error(new("SpaceTime", dates = dates, coord = coord),
               "NaN values are not allowed")
  # Try character
  dates[[1]][1] <- "A"
  coord[[1]][1] <- "A"
  expect_error(new("SpaceTime", dates = dates, coord = coord),
               "should be of numeric type")
  # Check length
  dates[[1]] <- 1:5
  coord[[1]] <- 1:7
  expect_error(new("SpaceTime", dates = dates, coord = coord),
               "should have the same length")
  expect_error(new("SpaceTime", epsg = 1:2),
               "should be a vector of length 1")
  # Check names
  names(dates) <- NULL
  names(coord) <- NULL
  expect_error(new("SpaceTime", dates = dates, coord = coord),
               "should have the following names")
})
# Matrix =======================================================================
test_that("Initialize a Matrix instance", {
  # Empty instance
  expect_s4_class(new("Matrix"), "Matrix")
  expect_message(new("Matrix"), "Matrix")

  MTX1 <- new("Matrix", data = mtx_count)
  expect_s4_class(MTX1, "Matrix")
  expect_identical(MTX1@cases, LETTERS[1:10])
  expect_identical(MTX1@types, LETTERS[26:17])
  expect_identical(dimnames(MTX1@.Data), list(LETTERS[1:10], LETTERS[26:17]))

  MTX2 <- new("Matrix", data = mtx_count,
              cases = letters[26:17],
              types = letters[1:10])
  expect_s4_class(MTX2, "Matrix")
  expect_identical(MTX2@cases, letters[26:17])
  expect_identical(MTX2@types, letters[1:10])
  expect_identical(dimnames(MTX2@.Data), list(letters[26:17], letters[1:10]))

  # Test length
  expect_error(new("Matrix", data = mtx_count,
                   cases = c(LETTERS),
                   types = c(letters)),
               "should be a vector of length")
  # Try NA
  expect_error(new("Matrix", data = mtx_count,
                   cases = c(1:9, NA),
                   types = c(1:9, NA)),
               "Missing values are not allowed")
})
# Numeric matrix ===============================================================
test_that("Initialize a NumericMatrix instance", {
  # Empty instence
  expect_s4_class(new("NumericMatrix"), "NumericMatrix")

  expect_s4_class(new("NumericMatrix", mtx_count), "NumericMatrix")
  expect_s4_class(new("NumericMatrix", mtx_freq), "NumericMatrix")

  # Try logical
  expect_error(new("NumericMatrix", mtx_incid),
               "Numeric values are expected.")
  # Try character
  expect_error(new("NumericMatrix", mtx_character),
               "Numeric values are expected.")
  # Try NA
  expect_error(new("NumericMatrix", mtx_num_na),
               "Missing or infinite values were detected.")
  # Try Inf
  expect_error(new("NumericMatrix", mtx_num_inf),
               "Missing or infinite values were detected.")
  # Try NaN
  expect_error(new("NumericMatrix", mtx_num_nan),
               "Missing or infinite values were detected.")
})
## Count matrix ----------------------------------------------------------------
test_that("Initialize a CountMatrix instance", {
  # Empty instence
  expect_s4_class(new("CountMatrix"), "CountMatrix")
  expect_message(new("CountMatrix"))

  dates <- list(value = 1:10, error = 1:10)
  count_matrix <- new("CountMatrix", data = mtx_count, dates = dates)
  expect_s4_class(count_matrix, "CountMatrix")
  expect_identical(count_matrix@dates, dates)

  # Try negative values
  expect_error(new("CountMatrix", data = -mtx_count),
               "Positive values are expected.")
  # Try relative frequencies
  expect_error(new("CountMatrix", data = mtx_freq),
               "Whole numbers are expected.")
  # Try logical
  expect_error(new("CountMatrix", data = mtx_incid),
               "Numeric values are expected.")
  # Try character
  expect_error(new("CountMatrix", data = mtx_character),
               "Numeric values are expected.")
  # Try NA
  expect_error(new("CountMatrix", data = mtx_num_na),
               "Missing or infinite values were detected.")
  # Try Inf
  expect_error(new("CountMatrix", data = mtx_num_inf),
               "Missing or infinite values were detected.")
  # Try NaN
  expect_error(new("CountMatrix", data = mtx_num_nan),
               "Missing or infinite values were detected.")
})
## Frequency matrix ------------------------------------------------------------
test_that("Initialize a FrequencyMatrix instance", {
  expect_s4_class(new("FrequencyMatrix"), "FrequencyMatrix")
  expect_message(new("FrequencyMatrix"))

  expect_s4_class(new("FrequencyMatrix", data = mtx_freq,
                      totals = rowSums(mtx_freq)), "FrequencyMatrix")

  expect_error(new("FrequencyMatrix", data = mtx_freq, total = 1)) # wrong total
  expect_error(new("FrequencyMatrix", data = mtx_freq)) # missing total
  expect_error(new("FrequencyMatrix", data = mtx_count)) # count data
  expect_error(new("FrequencyMatrix", data = mtx_incid)) # logical
})

# Logical matrix ===============================================================
test_that("Initialize a LogicalMatrix instance", {
  expect_s4_class(new("LogicalMatrix"), "LogicalMatrix")
  expect_s4_class(new("LogicalMatrix", data = mtx_incid), "LogicalMatrix")

  expect_error(new("LogicalMatrix", data = mtx_count)) # count data
  expect_error(new("LogicalMatrix", data = mtx_freq)) # frequency data
  expect_error(new("LogicalMatrix", data = mtx_character)) # character
  expect_error(new("LogicalMatrix", data = mtx_logic_na)) # NA
})
test_that("Initialize a IncidenceMatrix instance", {
  expect_s4_class(new("IncidenceMatrix"), "IncidenceMatrix")
  expect_message(new("IncidenceMatrix"))

  expect_s4_class(new("IncidenceMatrix", data = mtx_incid), "IncidenceMatrix")
  expect_s4_class(IncidenceMatrix(as.logical(sample(0:1, 100, TRUE)), ncol = 10), "IncidenceMatrix")

  expect_error(new("IncidenceMatrix", data = mtx_count)) # count data
  expect_error(new("IncidenceMatrix", data = mtx_freq)) # frequency data
})
