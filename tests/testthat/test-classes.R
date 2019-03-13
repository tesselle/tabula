context("Classes initialization")
options("verbose" = TRUE)

test_count <- matrix(sample(1:100, 100, TRUE), ncol = 10)
test_freq <- test_count / rowSums(test_count)
test_incid <- matrix(as.logical(sample(0:1, 100, TRUE, prob = c(0.25, 0.75))),
                     ncol = 10)
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
  expect_s4_class(new("SimilarityMatrix"), "SimilarityMatrix")
  expect_message(new("SimilarityMatrix"))
})
test_that("Initialize a NumericMatrix instance", {
  expect_s4_class(new("NumericMatrix", test_count), "NumericMatrix")
  expect_s4_class(new("NumericMatrix", test_freq), "NumericMatrix")

  expect_error(new("NumericMatrix", test_incid)) # logical
  expect_error(new("NumericMatrix", test_character)) # character
  expect_error(new("NumericMatrix", test_num_na)) # NA
  expect_error(new("NumericMatrix", test_num_inf)) # Inf
  expect_error(new("NumericMatrix", test_num_nan)) # NaN
})
test_that("Initialize a CountMatrix instance", {
  expect_s4_class(new("CountMatrix", test_count), "CountMatrix")
  expect_s4_class(CountMatrix(sample(1:100, 100, TRUE), ncol = 10), "CountMatrix")
  expect_s4_class(CountMatrix(sample(1:100, 100, TRUE),
                              ncol = 10, dimnames = list(NULL, 1:10)), "CountMatrix")
  expect_s4_class(CountMatrix(sample(1:100, 100, TRUE),
                              ncol = 10, dimnames = list(1:10, NULL)), "CountMatrix")

  expect_error(new("CountMatrix", -test_count)) # negative values
  expect_error(new("CountMatrix", -test_freq)) # negative values
  expect_error(new("CountMatrix", test_incid)) # logical
  expect_error(new("CountMatrix", test_character)) # character
  expect_error(new("CountMatrix", test_num_na)) # NA
  expect_error(new("CountMatrix", test_num_inf)) # Inf
  expect_error(new("CountMatrix", test_num_nan)) # NaN
})
test_that("Initialize a FrequencyMatrix instance", {
  expect_s4_class(new("FrequencyMatrix", test_freq,
                      totals = rowSums(test_freq)), "FrequencyMatrix")

  expect_error(new("FrequencyMatrix", test_freq, total = 1)) # wrong total
  expect_error(new("FrequencyMatrix", test_freq)) # missing total
  expect_error(new("FrequencyMatrix", test_count)) # count data
  expect_error(new("FrequencyMatrix", test_incid)) # logical
})

# Logical matrix ===============================================================
test_that("Initialize an empty logical matrix", {
  expect_s4_class(new("LogicalMatrix"), "LogicalMatrix")
  expect_s4_class(new("IncidenceMatrix"), "IncidenceMatrix")
  expect_message(new("IncidenceMatrix"))
})
test_that("Initialize a LogicalMatrix instance", {
  expect_s4_class(new("LogicalMatrix", test_incid), "LogicalMatrix")

  expect_error(new("LogicalMatrix", test_count)) # count data
  expect_error(new("LogicalMatrix", test_freq)) # frequency data
  expect_error(new("LogicalMatrix", test_character)) # character
  expect_error(new("LogicalMatrix", test_logic_na)) # NA
})
test_that("Initialize a IncidenceMatrix instance", {
  expect_s4_class(new("IncidenceMatrix", test_incid), "IncidenceMatrix")
  expect_s4_class(IncidenceMatrix(as.logical(sample(0:1, 100, TRUE)), ncol = 10), "IncidenceMatrix")

  expect_error(new("IncidenceMatrix", test_count)) # count data
  expect_error(new("IncidenceMatrix", test_freq)) # frequency data
})
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
        rows = data.frame("id" = LETTERS, "x" = 1:26, "y" = 1:26),
        columns = data.frame("id" = LETTERS, "x" = 1:26, "y" = 1:26),
        length = data.frame("id" = LETTERS, "d" = 1:26),
        cutoff = 3,
        keep = 1:3),
    "BootCA"
  )

  expect_error(new("BootCA", rows = data.frame(1:5)))
  expect_error(new("BootCA", rows = data.frame("id" = 1:26, "m" = 1:26, "n" = 1:26)))
  expect_error(new("BootCA", rows = data.frame("id" = 1:26, "x" = LETTERS, "y" = 1:26)))

  expect_error(new("BootCA", columns = data.frame(1:5)))
  expect_error(new("BootCA", columns = data.frame("id" = 1:26, "m" = 1:26, "n" = 1:26)))
  expect_error(new("BootCA", columns = data.frame("id" = 1:26, "x" = LETTERS, "y" = 1:26)))

  expect_error(new("BootCA", lengths = LETTERS))
  expect_error(new("BootCA", lengths = data.frame("id" = 1:26, "m" = 1:26)))
  expect_error(new("BootCA", lengths = data.frame("id" = 1:26, "d" = LETTERS)))

  expect_error(new("BootCA", cutoff = 1:2))
  expect_error(new("BootCA", keep = LETTERS))
})
# Dating =======================================================================
test_that("Initialize an empty DateModel object", {
  expect_s4_class(new("DateModel"), "DateModel")
})
test_that("Initialize a DateModel object", {
  #TODO
})
test_that("Initialize an empty BootDate object", {
  expect_s4_class(new("BootDate"), "BootDate")
})
test_that("Initialize a BootDate object", {
  expect_s4_class(
    new("BootDate",
        jackknife = data.frame(1:10, 1:10, 1:10, 1:10, 1:10, 1:10),
        bootstrap = data.frame(1:10, 1:10, 1:10, 1:10, 1:10, 1:10)),
    "BootDate"
  )

  expect_error(new("BootDate", jackknife = data.frame(1:5)))
  expect_error(new("BootDate", jackknife = data.frame(1:10, NA, 1:10, 1:10, 1:10, 1:10)))
  expect_error(new("BootDate", jackknife = data.frame(1:26, LETTERS, 1:26, 1:26, 1:26, 1:26)))

  expect_error(new("BootDate", bootstrap = data.frame(1:5)))
  expect_error(new("BootDate", bootstrap = data.frame(1:10, NA, 1:10, 1:10, 1:10, 1:10)))
  expect_error(new("BootDate", bootstrap = data.frame(1:26, LETTERS, 1:26, 1:26, 1:26, 1:26)))

  expect_error(new("BootDate",
                   jackknife = data.frame(1:10, 1:10, 1:10, 1:10, 1:10, 1:10),
                   bootstrap = data.frame(1:5, 1:5, 1:5, 1:5, 1:5, 1:5)))
})
