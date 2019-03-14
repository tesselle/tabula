# COERCION
#' @include AllClasses.R
NULL

## From NumericMatrix ==========================================================
setAs(from = "NumericMatrix", to = "data.frame", def = function(from)
  as.data.frame(methods::S3Part(from, strictS3 = TRUE, "matrix")))

## To CountMatrix --------------------------------------------------------------
matrix2count <- function(from) {
  data <- data.matrix(from)
  integer <- apply(X = data, MARGIN = 2, FUN = as.integer)
  dimnames(integer) <- dimnames(data)
  object <- methods::new("CountMatrix", integer)
  methods::validObject(object)
  return(object)
}
setAs(from = "matrix", to = "CountMatrix", def = matrix2count)
setAs(from = "data.frame", to = "CountMatrix", def = matrix2count)

## To FrequencyMatrix ----------------------------------------------------------
matrix2frequency <- function(from) {
  data <- data.matrix(from)
  totals <- rowSums(data)
  freq <- data / totals
  dimnames(freq) <- dimnames(data)
  object <- methods::new("FrequencyMatrix", freq, totals = totals)
  methods::validObject(object)
  return(object)
}
setAs(from = "matrix", to = "FrequencyMatrix", def = matrix2frequency)
setAs(from = "data.frame", to = "FrequencyMatrix", def = matrix2frequency)

## To SimilarityMatrix ----------------------------------------------------------
matrix2similarity <- function(from) {
  data <- data.matrix(from)
  rownames(data) <- colnames(from)
  object <- methods::new("SimilarityMatrix", data, method = "unknown")
  methods::validObject(object)
  return(object)
}
setAs(from = "matrix", to = "SimilarityMatrix", def = matrix2similarity)
setAs(from = "data.frame", to = "SimilarityMatrix", def = matrix2similarity)

## CountMatrix <> FrequencyMatrix ----------------------------------------------
setAs(
  from = "CountMatrix",
  to = "FrequencyMatrix",
  def = function(from) {
    counts <- methods::S3Part(from, strictS3 = TRUE, "matrix")
    totals <- rowSums(counts)
    freq <- counts / totals
    object <- methods::new("FrequencyMatrix", freq, totals = totals)
    methods::validObject(object)
    return(object)
  }
)
setAs(
  from = "FrequencyMatrix",
  to = "CountMatrix",
  def = function(from) {
    freq <- methods::S3Part(from, strictS3 = TRUE, "matrix")
    totals <- from@totals
    count <- round(freq * totals, digits = 0)
    integer <- apply(X = count, MARGIN = 2, FUN = as.integer)
    dimnames(integer) <- dimnames(freq)
    object <- methods::new("CountMatrix", integer)
    methods::validObject(object)
    return(object)
  }
)

## From LogicalMatrix ==========================================================
setAs(from = "LogicalMatrix", to = "data.frame", def = function(from)
  as.data.frame(methods::S3Part(from, strictS3 = TRUE, "matrix")))

## To IncidenceMatrix ----------------------------------------------------------
matrix2incidence <- function(from) {
  data <- if (isS4(from)) methods::as(from, "matrix")
  else data.matrix(from)
  data <- data > 0
  object <- methods::new("IncidenceMatrix", data)
  methods::validObject(object)
  return(object)
}
setAs(from = "matrix", to = "IncidenceMatrix", def = matrix2incidence)
setAs(from = "data.frame", to = "IncidenceMatrix", def = matrix2incidence)

setAs(from = "CountMatrix", to = "IncidenceMatrix", def = matrix2incidence)
setAs(from = "FrequencyMatrix", to = "IncidenceMatrix", def = matrix2incidence)

## To OccurrenceMatrix ---------------------------------------------------------
matrix2occurrence <- function(from) {
  data <- if (isS4(from)) {
    methods::as(from, "matrix")
  } else {
    data.matrix(from)
  }
  data <- data > 0
  p <- ncol(data)
  m <- nrow(data)
  labels <- if (is.null(colnames(data))) {
    paste("V", 1:p, sep = "")
  } else {
    colnames(data)
  }

  # @param indices A length-two numeric vector
  # @param data A numeric or logical matrix
  fun <- function(indices, data) {
    sum(data[, indices[1]] + data[, indices[2]] == 2)
  }
  # Get all combinations of variables, taken 2 at a time
  combine <- utils::combn(1:p, 2, simplify = TRUE)
  occurrence <- apply(X = combine, MARGIN = 2, FUN = fun, data = data) / m

  C <- matrix(data = FALSE, nrow = p, ncol = p, dimnames = list(labels, labels))
  C[lower.tri(C, diag = FALSE)] <- occurrence
  C <- t(C)
  C[lower.tri(C, diag = FALSE)] <- occurrence

  object <- methods::new("OccurrenceMatrix", C)
  methods::validObject(object)
  return(object)
}

setAs(from = "matrix", to = "OccurrenceMatrix", def = matrix2occurrence)
setAs(from = "data.frame", to = "OccurrenceMatrix", def = matrix2occurrence)

setAs(from = "CountMatrix", to = "OccurrenceMatrix", def = matrix2occurrence)
setAs(from = "FrequencyMatrix", to = "OccurrenceMatrix", def = matrix2occurrence)
setAs(from = "IncidenceMatrix", to = "OccurrenceMatrix", def = matrix2occurrence)
