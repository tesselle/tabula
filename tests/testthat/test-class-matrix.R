context("Matrix classes")
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

# Create a logical matrix with NA, Inf and NaN values
mtx_logic_na <- mtx_logic_inf <- mtx_logic_nan <- mtx_incid
mtx_logic_na[sample(1:100, 10, FALSE)] <- NA
mtx_logic_inf[sample(1:100, 10, FALSE)] <- Inf
mtx_logic_nan[sample(1:100, 10, FALSE)] <- NaN

# Space-Time ===================================================================
test_that("Initialize a SpaceTime instance", {
  # Empty instence
  expect_s4_class(.SpaceTime(), "SpaceTime")

  dates <- list(value = 1:10, error = 1:10)
  coord <- list(x = 1:10, y = 1:10, z = 1:10)
  expect_s4_class(SpaceTime(dates = dates, coordinates = coord, epsg = 4326),
                  "SpaceTime")
  expect_message(SpaceTime(dates = dates, coordinates = coord, epsg = 4326),
                 "SpaceTime")

  # Try NA
  dates[[1]][1] <- NA_real_
  coord[[1]][1] <- NA_real_
  expect_s4_class(SpaceTime(dates = dates, coordinates = coord), "SpaceTime")
  expect_error(SpaceTime(epsg = NA), "a missing value was detected")

  # Try Inf
  dates[[1]][1] <- Inf
  coord[[1]][1] <- Inf
  expect_error(SpaceTime(dates = dates, coordinates = coord),
               "an infinite value was detected")
  # Try character
  dates[[1]][1] <- "A"
  coord[[1]][1] <- "A"
  expect_error(SpaceTime(dates = dates, coordinates = coord),
               "should be of numeric type")
  # Check length
  dates[[1]] <- 1:5
  coord[[1]] <- 1:7
  expect_error(SpaceTime(dates = dates, coordinates = coord),
               "should have the same length")
  expect_error(SpaceTime(epsg = 1:2),
               "should be of length 1, not 2")
  # Check names
  names(dates) <- NULL
  names(coord) <- NULL
  expect_error(SpaceTime(dates = dates, coordinates = coord),
               "should have the following names")
})
# Matrix =======================================================================
test_that("Initialize a Matrix instance", {
  # Empty instance
  expect_s4_class(.Matrix(matrix(0, 1, 1)), "Matrix")
  expect_message(Matrix(data = 0))

  MTX1 <- Matrix(data = mtx_count)
  expect_s4_class(MTX1, "Matrix")
  expect_identical(dimnames(MTX1), list(LETTERS[1:10], LETTERS[26:17]))

  expect_error(.Matrix(matrix(NA, 1, 1)),
               "a missing value was detected")
  expect_error(.Matrix(matrix(NaN, 1, 1)),
               "a non-number value was detected")
  expect_error(.Matrix(matrix(Inf, 1, 1)),
               "an infinite value was detected")
  expect_error(.Matrix(id = NA_character_),
               "a missing value was detected")
  expect_error(.Matrix(id = LETTERS),
               "should be of length 1, not 26")
  expect_error(.Matrix(id = "a"),
               "should be 36 characters long string, not 1")
})
# Numeric matrix ===============================================================
test_that("Initialize a NumericMatrix instance", {
  # Empty instence
  expect_s4_class(.NumericMatrix(), "NumericMatrix")
  expect_s4_class(NumericMatrix(), "NumericMatrix")

  expect_s4_class(NumericMatrix(mtx_count), "NumericMatrix")
  expect_s4_class(NumericMatrix(mtx_freq), "NumericMatrix")

  # Try logical
  expect_error(NumericMatrix(mtx_incid),
               "should be of numeric type")
  # Try character
  expect_error(NumericMatrix(mtx_character),
               "should be of numeric type")
  # Try NA
  expect_error(NumericMatrix(mtx_num_na),
               "10 missing values were detected")
  # Try NaN
  expect_error(NumericMatrix(mtx_num_nan),
               "10 non-number values were detected")
  # Try Inf
  expect_error(NumericMatrix(mtx_num_inf),
               "10 infinite values were detected")
})
## Count matrix ----------------------------------------------------------------
test_that("Initialize a CountMatrix instance", {
  # Empty instence
  expect_s4_class(.CountMatrix(), "CountMatrix")
  expect_s4_class(.CountMatrix(mtx_count), "CountMatrix")

  # Try negative values
  expect_error(.CountMatrix(-mtx_count),
               "positive values are expected")
  # Try relative frequencies
  expect_error(.CountMatrix(mtx_freq),
               "whole numbers are expected")
  # Try logical
  expect_error(.CountMatrix(mtx_incid),
               "should be of numeric type")
  # Try character
  expect_error(.CountMatrix(mtx_character),
               "should be of numeric type")
  # Try NA
  expect_error(.CountMatrix(mtx_num_na),
               "10 missing values were detected")
  # Try NaN
  expect_error(.CountMatrix(mtx_num_nan),
               "10 non-number values were detected")
  # Try Inf
  expect_error(.CountMatrix(mtx_num_inf),
               "10 infinite values were detected")
})
test_that("CountMatrix constructor", {
  expect_message(CountMatrix())
  count_matrix1 <- CountMatrix(
    data = sample(0:10, 100, TRUE),
    ncol = 20
  )
  expect_equal(dim(count_matrix1), c(5, 20))
  expect_equal(dimnames(count_matrix1),
               list(as.character(1:5), paste0("V", 1:20)))

  count_matrix2 <- CountMatrix(
    data = sample(0:10, 100, TRUE),
    nrow = 20,
    dimnames = list(NULL, LETTERS[1:5])
  )
  expect_equal(dim(count_matrix2), c(20, 5))
  expect_equal(dimnames(count_matrix2),
               list(as.character(1:20), LETTERS[1:5]))
})
## Frequency matrix ------------------------------------------------------------
test_that("Initialize a FrequencyMatrix instance", {
  expect_s4_class(.FrequencyMatrix(), "FrequencyMatrix")
  expect_s4_class(.FrequencyMatrix(mtx_freq, totals = rowSums(mtx_freq)),
                  "FrequencyMatrix")

  # Try wrong total
  expect_error(.FrequencyMatrix(mtx_freq, totals = 1),
               "should be of length 10, not 1")
  # Try missing total
  expect_error(.FrequencyMatrix(mtx_freq),
               "should be of length 10, not 0")
  # Try count data
  expect_error(.FrequencyMatrix(mtx_count),
               "should have constant row sums")
  # Try logical
  expect_error(.FrequencyMatrix(mtx_incid),
               "should be of numeric type")
})

# Logical matrix ===============================================================
test_that("Initialize a LogicalMatrix instance", {
  expect_s4_class(.LogicalMatrix(), "LogicalMatrix")
  expect_message(LogicalMatrix())
  expect_s4_class(LogicalMatrix(mtx_incid), "LogicalMatrix")

  # Try count data
  expect_error(LogicalMatrix(data = mtx_count),
               "should be of logical type")
  # Try frequency data
  expect_error(LogicalMatrix(data = mtx_freq),
               "should be of logical type")
  # Try character
  expect_error(LogicalMatrix(data = mtx_character),
               "should be of logical type")
  # Try NA
  expect_error(LogicalMatrix(mtx_logic_na),
               "10 missing values were detected")
  # Try NaN
  expect_error(LogicalMatrix(mtx_logic_nan),
               "10 non-number values were detected")
  # Try Inf
  expect_error(LogicalMatrix(mtx_logic_inf),
               "10 infinite values were detected")
})
test_that("Initialize a IncidenceMatrix instance", {
  expect_s4_class(.IncidenceMatrix(), "IncidenceMatrix")
  expect_s4_class(.IncidenceMatrix(mtx_incid), "IncidenceMatrix")

  # Try count data
  expect_error(.IncidenceMatrix(mtx_count),
               "should be of logical type")
  # Try frequency data
  expect_error(.IncidenceMatrix(mtx_freq),
               "should be of logical type")
  # Try character
  expect_error(.IncidenceMatrix(mtx_character),
               "should be of logical type")
  # Try NA
  expect_error(.IncidenceMatrix(mtx_logic_na),
               "10 missing values were detected")
  # Try NaN
  expect_error(.IncidenceMatrix(mtx_logic_nan),
               "10 non-number values were detected")
  # Try Inf
  expect_error(.IncidenceMatrix(mtx_logic_inf),
               "10 infinite values were detected")
})
test_that("IncidenceMatrix constructor", {
  incid_matrix1 <- IncidenceMatrix(
    data = sample(0:1, 100, TRUE),
    ncol = 20
  )
  expect_equal(dim(incid_matrix1), c(5, 20))
  expect_equal(dimnames(incid_matrix1),
               list(as.character(1:5), paste0("V", 1:20)))

  incid_matrix2 <- IncidenceMatrix(
    data = as.logical(sample(0:1, 100, TRUE)),
    nrow = 20,
    dimnames = list(LETTERS[1:20], NULL)
  )
  expect_equal(dim(incid_matrix2), c(20, 5))
  expect_equal(dimnames(incid_matrix2),
               list(LETTERS[1:20], paste0("V", 1:5)))
})
