context("Classes initialization")
library(tabula)
options("verbose" = TRUE)

test_count <- matrix(sample(1:100, 100, TRUE), ncol = 10)
test_freq <- test_count / rowSums(test_count)
test_incid <- matrix(as.logical(sample(0:1, 100, TRUE)), ncol = 10)
test_character <- matrix(sample(LETTERS, 100, TRUE), ncol = 10)
test_num_na <- test_num_inf <- test_num_nan <- test_count
test_num_na[sample(1:100, 10, FALSE)] <- NA
test_num_inf[sample(1:100, 10, FALSE)] <- Inf
test_num_nan[sample(1:100, 10, FALSE)] <- NaN
test_logic_na <- test_incid
test_logic_na[sample(1:100, 10, FALSE)] <- NA

# Numeric matrix ===============================================================
test_that("Initialize an empty numeric matrix", {
  expect_s4_class(new("NumericMatrix"), "NumericMatrix")
  expect_s4_class(new("CountMatrix"), "CountMatrix")
  expect_message(new("CountMatrix"))
  expect_s4_class(new("FrequencyMatrix"), "FrequencyMatrix")
  expect_message(new("FrequencyMatrix"))
})
test_that("Initialize a NumericMatrix instance", {
  expect_s4_class(new("NumericMatrix", test_count), "NumericMatrix")
  expect_s4_class(new("NumericMatrix", test_freq), "NumericMatrix")

  expect_error(new("NumericMatrix", -test_count))
  expect_error(new("NumericMatrix", -test_freq))
  expect_error(new("NumericMatrix", test_incid))
  expect_error(new("NumericMatrix", test_character))
  expect_error(new("NumericMatrix", test_num_na))
  expect_error(new("NumericMatrix", test_num_inf))
  expect_error(new("NumericMatrix", test_num_nan))
})
test_that("Initialize a CountMatrix instance", {
  expect_s4_class(new("CountMatrix", test_count), "CountMatrix")
  expect_s4_class(CountMatrix(sample(1:100, 100, TRUE), ncol = 10), "CountMatrix")

  expect_error(new("CountMatrix", -test_count))
  expect_error(new("CountMatrix", -test_freq))
  expect_error(new("CountMatrix", test_incid))
  expect_error(new("CountMatrix", test_character))
  expect_error(new("CountMatrix", test_num_na))
  expect_error(new("CountMatrix", test_num_inf))
  expect_error(new("CountMatrix", test_num_nan))
})
test_that("Initialize a FrequencyMatrix instance", {
  expect_s4_class(new("FrequencyMatrix", test_freq,
                      totals = rowSums(test_freq)), "FrequencyMatrix")
  expect_is(totals(new("FrequencyMatrix", test_freq,
                       totals = rowSums(test_freq))), "numeric")

  expect_error(new("FrequencyMatrix", test_freq, total = 1)) # Wrong total
  expect_error(new("FrequencyMatrix", test_freq)) # Missing total
  expect_error(new("FrequencyMatrix", test_count))
  expect_error(new("FrequencyMatrix", test_incid))
})

# Logical matrix ===============================================================
test_that("Initialize an empty logical matrix", {
  expect_s4_class(new("LogicalMatrix"), "LogicalMatrix")
  expect_s4_class(new("IncidenceMatrix"), "IncidenceMatrix")
  expect_message(new("IncidenceMatrix"))
})
test_that("Initialize a LogicalMatrix instance", {
  expect_s4_class(new("LogicalMatrix", test_incid), "LogicalMatrix")

  expect_error(new("LogicalMatrix", test_count))
  expect_error(new("LogicalMatrix", test_freq))
  expect_error(new("LogicalMatrix", test_character))
  expect_error(new("LogicalMatrix", test_logic_na))
})
test_that("Initialize a IncidenceMatrix instance", {
  expect_s4_class(new("IncidenceMatrix", test_incid), "IncidenceMatrix")
  expect_s4_class(IncidenceMatrix(as.logical(sample(0:1, 100, TRUE)), ncol = 10), "IncidenceMatrix")

  expect_error(new("IncidenceMatrix", test_count))
  expect_error(new("IncidenceMatrix", test_freq))
})
# Seriation ====================================================================
test_that("Initialize an empty PermutationOrder object", {
  expect_s4_class(new("PermutationOrder"), "PermutationOrder")
})
test_that("Access PermutationOrder slots", {
  df <- data.frame("id" = LETTERS, "x" = 1:26, "y" = 1:26)
  boot <- new("PermutationOrder", rows = 1:5, columns = 1:5, method = "X")

  expect_is(rows(boot), "integer")
  expect_is(columns(boot), "integer")
  expect_is(boot[["rows"]], "integer")
  expect_is(boot[["columns"]], "integer")
  expect_is(boot[["method"]], "character")
})
test_that("Initialize an empty BootCA object", {
  expect_s4_class(new("BootCA"), "BootCA")
})
test_that("Initialize a BootCA object", {
  df <- data.frame("id" = LETTERS, "x" = 1:26, "y" = 1:26)
  expect_s4_class(new("BootCA", rows = df, columns = df, length = 1:26,
                      cutoff = 3, keep = 1:3), "BootCA")

  expect_error(new("BootCA", rows = data.frame(1:5)))
  expect_error(new("BootCA", rows = data.frame("id" = 1:26, "m" = 1:26,
                                               "n" = 1:26)))
  expect_error(new("BootCA", rows = data.frame("id" = 1:26, "x" = LETTERS,
                                               "y" = 1:26)))
  expect_error(new("BootCA", columns = data.frame(1:5)))
  expect_error(new("BootCA", columns = data.frame("id" = 1:26, "m" = 1:26,
                                                  "n" = 1:26)))
  expect_error(new("BootCA", columns = data.frame("id" = 1:26, "x" = LETTERS,
                                                  "y" = 1:26)))
  expect_error(new("BootCA", cutoff = 1:2))
  expect_error(new("BootCA", lengths = LETTERS))
})
test_that("Access BootCA slots", {
  df <- data.frame("id" = LETTERS, "x" = 1:26, "y" = 1:26)
  boot <- new("BootCA", rows = df, columns = df, length = 1:26,
              cutoff = 3, keep = 1:3)

  expect_is(boot[["rows"]], "data.frame")
  expect_is(boot[["columns"]], "data.frame")
  expect_is(boot[["lengths"]], "integer")
  expect_is(boot[["cutoff"]], "numeric")
  expect_is(boot[["keep"]], "integer")
})
