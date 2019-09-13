context("Matrix classes")

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

  dates <- cbind(value = 1:10, error = 1:10)
  coord <- cbind(X = 1:10, Y = 1:10, Z = 1:10)
  # Try Inf
  dates[1, 1] <- Inf
  coord[1, 1] <- Inf
  cnd <- catch_conditions(.SpaceTime(dates = dates, coordinates = coord))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must not contain infinite values", cnd[[1]]$message))
  # Try character
  dates[1, 1] <- "A"
  coord[1, 1] <- "A"
  cnd <- catch_conditions(.SpaceTime(dates = dates, coordinates = coord))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must be numeric; not character", cnd[[1]]$message))
  # Check length
  cnd <- catch_conditions(.SpaceTime(epsg = 1:2))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must be a scalar", cnd[[1]]$message))
  # Check names
  colnames(dates) <- NULL
  colnames(coord) <- NULL
  cnd <- catch_conditions(.SpaceTime(dates = dates, coordinates = coord))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must have the following column names", cnd[[1]]$message))
})
test_that("SpaceTime constructor", {
  options("verbose" = TRUE)
  cnd <- catch_conditions(SpaceTime())
  expect_length(cnd, 1)
  expect_s3_class(cnd[[1]], "message_class_initialize")

  dates <- cbind(value = 1:10, error = 1:10)
  coord <- cbind(X = 1:10, Y = 1:10, Z = 1:10)
  expect_s4_class(SpaceTime(dates = dates, coordinates = coord, epsg = 4326),
                  "SpaceTime")
})

# Matrix =======================================================================
test_that("Initialize a Matrix instance", {
  # Empty instence
  expect_s4_class(.Matrix(), "Matrix")

  cnd <- catch_conditions(.Matrix(matrix(NA, 1, 1)))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))

  cnd <- catch_conditions(.Matrix(matrix(NaN, 1, 1)))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))

  cnd <- catch_conditions(.Matrix(matrix(Inf, 1, 1)))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must not contain infinite values", cnd[[1]]$message))

  cnd <- catch_conditions(.Matrix(id = NA_character_))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must be an UUID", cnd[[1]]$message))

  cnd <- catch_conditions(.Matrix(id = "a"))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must be an UUID", cnd[[1]]$message))
})
test_that("Matrix constructor", {
  options("verbose" = TRUE)
  cnd <- catch_conditions(Matrix())
  expect_length(cnd, 2)
  expect_s3_class(cnd[[1]], "message_class_initialize")

  mtx <- Matrix(data = mtx_count)
  expect_s4_class(mtx, "Matrix")
  expect_identical(dimnames(mtx), list(LETTERS[1:10], LETTERS[26:17]))
})

# Numeric matrix ===============================================================
test_that("Initialize a NumericMatrix instance", {
  # Empty instence
  expect_s4_class(.NumericMatrix(), "NumericMatrix")
  expect_s4_class(.NumericMatrix(mtx_count), "NumericMatrix")
  expect_s4_class(.NumericMatrix(mtx_freq), "NumericMatrix")

  # Try logical
  cnd <- catch_conditions(.NumericMatrix(mtx_incid))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must be numeric; not logical", cnd[[1]]$message))
  # Try character
  cnd <- catch_conditions(.NumericMatrix(mtx_character))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must be numeric; not character", cnd[[1]]$message))
  # Try NA
  cnd <- catch_conditions(.NumericMatrix(mtx_num_na))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try NaN
  cnd <- catch_conditions(.NumericMatrix(mtx_num_nan))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try Inf
  cnd <- catch_conditions(.NumericMatrix(mtx_num_inf))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must not contain infinite values", cnd[[1]]$message))
})
test_that("NumericMatrix constructor", {
  options("verbose" = TRUE)
  cnd <- catch_conditions(NumericMatrix())
  expect_length(cnd, 2)
  for (i in seq_len(2)) {
    expect_s3_class(cnd[[i]], "message_class_initialize")
  }
})

## Count matrix ----------------------------------------------------------------
test_that("Initialize a CountMatrix instance", {
  # Empty instence
  expect_s4_class(.CountMatrix(), "CountMatrix")
  expect_s4_class(.CountMatrix(mtx_count), "CountMatrix")

  # Try 0s and 1s
  expect_message(CountMatrix(as.numeric(mtx_incid), nrow = 10),
                 "You should consider using an incidence matrix instead.")
  # Try relative frequencies
  cnd <- catch_conditions(.CountMatrix(mtx_freq))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must contain whole numbers", cnd[[1]]$message))
  # Try logical
  cnd <- catch_conditions(.CountMatrix(mtx_incid))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must be numeric; not logical", cnd[[1]]$message))
  # Try negative values
  cnd <- catch_conditions(.CountMatrix(-mtx_count))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must contain positive numbers", cnd[[1]]$message))
  # Try character
  cnd <- catch_conditions(.CountMatrix(mtx_character))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must be numeric; not character", cnd[[1]]$message))
  # Try NA
  cnd <- catch_conditions(.CountMatrix(mtx_num_na))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try NaN
  cnd <- catch_conditions(.CountMatrix(mtx_num_nan))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try Inf
  cnd <- catch_conditions(.CountMatrix(mtx_num_inf))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must not contain infinite values", cnd[[1]]$message))
})
test_that("CountMatrix constructor", {
  options("verbose" = TRUE)
  cnd <- catch_conditions(CountMatrix())
  expect_length(cnd, 4)
  for (i in seq_len(3)) {
    expect_s3_class(cnd[[i]], "message_class_initialize")
  }

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
  cnd <- catch_conditions(.FrequencyMatrix(mtx_freq, totals = 1))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must be of length 10; not 1", cnd[[1]]$message))
  # Try missing total
  cnd <- catch_conditions(.FrequencyMatrix(mtx_freq))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must be of length 10; not 0", cnd[[1]]$message))
  # Try count data
  cnd <- catch_conditions(.FrequencyMatrix(mtx_count))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must have constant row sums", cnd[[1]]$message))
  # Try logical
  cnd <- catch_conditions(.FrequencyMatrix(mtx_incid))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must be numeric; not logical.", cnd[[1]]$message))
})

## Co-occurrenceMatrix matrix --------------------------------------------------
test_that("OccurrenceMatrix constructor", {
  options("verbose" = TRUE)
  cnd <- catch_conditions(OccurrenceMatrix())
  expect_length(cnd, 3)
  for (i in seq_len(3)) {
    expect_s3_class(cnd[[i]], "message_class_initialize")
  }
})

## Similarity matrix -----------------------------------------------------------
test_that("SimilarityMatrix constructor", {
  options("verbose" = TRUE)
  cnd <- catch_conditions(SimilarityMatrix())
  expect_length(cnd, 3)
  for (i in seq_len(3)) {
    expect_s3_class(cnd[[i]], "message_class_initialize")
  }
})

# Logical matrix ===============================================================
test_that("Initialize a LogicalMatrix instance", {
  expect_s4_class(.LogicalMatrix(), "LogicalMatrix")
  expect_s4_class(.LogicalMatrix(mtx_incid), "LogicalMatrix")

  # Try count data
  cnd <- catch_conditions(.LogicalMatrix(data = mtx_count))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must be logical; not integer", cnd[[1]]$message))
  # Try frequency data
  cnd <- catch_conditions(.LogicalMatrix(data = mtx_freq))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must be logical; not double", cnd[[1]]$message))
  # Try character
  cnd <- catch_conditions(.LogicalMatrix(data = mtx_character))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must be logical; not character", cnd[[1]]$message))
  # Try NA
  cnd <- catch_conditions(.LogicalMatrix(mtx_logic_na))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try NaN
  cnd <- catch_conditions(.LogicalMatrix(mtx_logic_nan))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try Inf
  cnd <- catch_conditions(.LogicalMatrix(mtx_logic_inf))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must not contain infinite values", cnd[[1]]$message))
})
test_that("LogicalMatrix constructor", {
  options("verbose" = TRUE)
  cnd <- catch_conditions(LogicalMatrix())
  expect_length(cnd, 2)
  for (i in seq_len(2)) {
    expect_s3_class(cnd[[i]], "message_class_initialize")
  }
})
test_that("Initialize a IncidenceMatrix instance", {
  expect_s4_class(.IncidenceMatrix(), "IncidenceMatrix")
  expect_s4_class(.IncidenceMatrix(mtx_incid), "IncidenceMatrix")

  # Try count data
  cnd <- catch_conditions(.IncidenceMatrix(mtx_count))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must be logical; not integer", cnd[[1]]$message))
  # Try frequency data
  cnd <- catch_conditions(.IncidenceMatrix(mtx_freq))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must be logical; not double", cnd[[1]]$message))
  # Try character
  cnd <- catch_conditions(.IncidenceMatrix(mtx_character))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must be logical; not character.", cnd[[1]]$message))
  # Try NA
  cnd <- catch_conditions(.IncidenceMatrix(mtx_logic_na))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try NaN
  cnd <- catch_conditions(.IncidenceMatrix(mtx_logic_nan))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must not contain missing values", cnd[[1]]$message))
  # Try Inf
  cnd <- catch_conditions(.IncidenceMatrix(mtx_logic_inf))
  expect_s3_class(cnd[[1]], "error_class_initialize")
  expect_true(grepl("must not contain infinite values", cnd[[1]]$message))
})
test_that("IncidenceMatrix constructor", {
  options("verbose" = TRUE)
  cnd <- catch_conditions(IncidenceMatrix())
  expect_length(cnd, 3)
  for (i in seq_len(3)) {
    expect_s3_class(cnd[[i]], "message_class_initialize")
  }

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
