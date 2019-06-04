# COERCION
#' @include AllClasses.R
NULL

## From NumericMatrix ==========================================================
setAs(from = "NumericMatrix", to = "data.frame", def = function(from)
  as.data.frame(methods::S3Part(from, strictS3 = TRUE, "matrix")))

## To CountMatrix ==============================================================
matrix2count <- function(from) {
  data <- data.matrix(from)
  whole_numbers <- apply(
    X = data,
    MARGIN = 2,
    FUN = function(x) as.integer(round(x, digits = 0))
  )
  dimnames(whole_numbers) <- dimnames(data)
  object <- .CountMatrix(whole_numbers)
  methods::validObject(object)
  return(object)
}
setAs(from = "matrix", to = "CountMatrix", def = matrix2count)
setAs(from = "data.frame", to = "CountMatrix", def = matrix2count)

## To FrequencyMatrix ==========================================================
matrix2frequency <- function(from) {
  data <- data.matrix(from)
  totals <- rowSums(data)
  freq <- data / totals
  dimnames(freq) <- dimnames(data)
  object <- .FrequencyMatrix(freq, totals = totals)
  methods::validObject(object)
  return(object)
}
setAs(from = "matrix", to = "FrequencyMatrix", def = matrix2frequency)
setAs(from = "data.frame", to = "FrequencyMatrix", def = matrix2frequency)

## To SimilarityMatrix =========================================================
matrix2similarity <- function(from) {
  data <- data.matrix(from)
  rownames(data) <- colnames(from)
  object <- .SimilarityMatrix(data, method = "unknown")
  methods::validObject(object)
  return(object)
}
setAs(from = "matrix", to = "SimilarityMatrix", def = matrix2similarity)
setAs(from = "data.frame", to = "SimilarityMatrix", def = matrix2similarity)

## CountMatrix <> FrequencyMatrix ==============================================
setAs(
  from = "CountMatrix",
  to = "FrequencyMatrix",
  def = function(from) {
    counts <- methods::S3Part(from, strictS3 = TRUE, "matrix")
    totals <- rowSums(counts)
    freq <- counts / totals
    object <- .FrequencyMatrix(freq, totals = totals)
    object@id <- from@id
    object@dates <- from@dates
    object@coordinates <- from@coordinates
    object@epsg <- from@epsg
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
    if (isEmpty(totals))
      stop("Cannot calculate absolute frequencies (`totals` is empty).",
           call. = FALSE)
    count <- round(freq * totals, digits = 0)
    integer <- apply(
      X = count,
      MARGIN = 2,
      FUN = function(x) as.integer(round(x, digits = 0))
    )
    dimnames(integer) <- dimnames(freq)
    object <- .CountMatrix(integer)
    object@id <- from@id
    object@dates <- from@dates
    object@coordinates <- from@coordinates
    object@epsg <- from@epsg
    methods::validObject(object)
    return(object)
  }
)

## From LogicalMatrix ==========================================================
setAs(from = "LogicalMatrix", to = "data.frame", def = function(from)
  as.data.frame(methods::S3Part(from, strictS3 = TRUE, "matrix")))

## To IncidenceMatrix ==========================================================
matrix2incidence <- function(from) {
  data <- if (isS4(from)) {
    methods::S3Part(from, strictS3 = TRUE, "matrix")
  } else {
    data.matrix(from)
  }
  data <- data > 0
  object <- .IncidenceMatrix(data)
  if (isS4(from)) {
    object@id <- from@id
    object@dates <- from@dates
    object@coordinates <- from@coordinates
    object@epsg <- from@epsg
  }
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
    methods::S3Part(from, strictS3 = TRUE, "matrix")
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

  object <- .OccurrenceMatrix(C)
  if (isS4(from)) object@id <- from@id
  methods::validObject(object)
  return(object)
}

setAs(from = "matrix", to = "OccurrenceMatrix", def = matrix2occurrence)
setAs(from = "data.frame", to = "OccurrenceMatrix", def = matrix2occurrence)

setAs(from = "CountMatrix", to = "OccurrenceMatrix", def = matrix2occurrence)
setAs(from = "FrequencyMatrix", to = "OccurrenceMatrix", def = matrix2occurrence)
setAs(from = "IncidenceMatrix", to = "OccurrenceMatrix", def = matrix2occurrence)
