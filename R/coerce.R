# COERCION
#' @include AllGenerics.R AllClasses.R
NULL

# \code{as_matrix} attempts to coerce \code{from} in a suitable way, i. e. to a
# \linkS4class{CountMatrix} or an \linkS4class{IncidenceMatrix}.
# @rdname coerce
# @aliases as_matrix,matrix-method
# setMethod(
#   f = "as_matrix",
#   signature = signature(from = "matrix"),
#   definition = function(from) {
#     if (!is.numeric(from))
#       stop("A numeric matrix is expected.", call. = FALSE)
#
#     if (all(is_binary(from))) {
#       methods::as(from, "IncidenceMatrix")
#     } else if (all(is_whole(from))) {
#       methods::as(from, "CountMatrix")
#     } else {
#       stop("Check your input data.", call. = FALSE)
#     }
#   }
# )

# @rdname coerce
# @aliases as_matrix,data.frame-method
# setMethod(
#   f = "as_matrix",
#   signature = signature(from = "data.frame"),
#   definition = function(from) {
#     if (!all(lapply(X = from, FUN = is.numeric)))
#       stop("A numeric data frame is expected.", call. = FALSE)
#
#     if (!all(lapply(X = from, FUN = function(x) all(is_binary(x))))) {
#       methods::as(from, "IncidenceMatrix")
#     } else if (!all(lapply(X = from, FUN = function(x) all(is_whole(x))))) {
#       methods::as(from, "CountMatrix")
#     } else {
#       stop("Check your input data.", call. = FALSE)
#     }
#   }
# )

#' @export
#' @rdname coerce
#' @aliases as_count,ANY-method
setMethod(
  f = "as_count",
  signature = signature(from = "ANY"),
  definition = function(from) {
    methods::as(from, "CountMatrix")
  }
)

#' @export
#' @rdname coerce
#' @aliases as_frequency,ANY-method
setMethod(
  f = "as_frequency",
  signature = signature(from = "ANY"),
  definition = function(from) {
    methods::as(from, "FrequencyMatrix")
  }
)

#' @export
#' @rdname coerce
#' @aliases as_incidence,ANY-method
setMethod(
  f = "as_incidence",
  signature = signature(from = "ANY"),
  definition = function(from) {
    methods::as(from, "IncidenceMatrix")
  }
)

#' @export
#' @rdname coerce
#' @aliases as_occurrence,ANY-method
setMethod(
  f = "as_occurrence",
  signature = signature(from = "ANY"),
  definition = function(from) {
    methods::as(from, "OccurrenceMatrix")
  }
)

#' @export
#' @rdname coerce
#' @aliases as_similarity,ANY-method
setMethod(
  f = "as_similarity",
  signature = signature(from = "ANY"),
  definition = function(from) {
    methods::as(from, "SimilarityMatrix")
  }
)

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
  .CountMatrix(whole_numbers, id = generate_uuid())
}
setAs(from = "matrix", to = "CountMatrix", def = matrix2count)
setAs(from = "data.frame", to = "CountMatrix", def = matrix2count)

## To FrequencyMatrix ==========================================================
matrix2frequency <- function(from) {
  data <- data.matrix(from)
  totals <- rowSums(data)
  freq <- data / totals
  dimnames(freq) <- dimnames(data)
  .FrequencyMatrix(freq, totals = totals, id = generate_uuid())
}
setAs(from = "matrix", to = "FrequencyMatrix", def = matrix2frequency)
setAs(from = "data.frame", to = "FrequencyMatrix", def = matrix2frequency)

## To SimilarityMatrix =========================================================
matrix2similarity <- function(from) {
  data <- data.matrix(from)
  rownames(data) <- colnames(from)
  .SimilarityMatrix(data, method = "unknown", id = generate_uuid())
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
    .FrequencyMatrix(
      freq,
      totals = totals,
      id = from@id,
      dates = from@dates,
      coordinates = from@coordinates,
      epsg = from@epsg
    )
  }
)
setAs(
  from = "FrequencyMatrix",
  to = "CountMatrix",
  def = function(from) {
    freq <- methods::S3Part(from, strictS3 = TRUE, "matrix")
    totals <- from@totals
    if (is_empty(totals))
      stop("Cannot calculate absolute frequencies (`totals` is empty).",
           call. = FALSE)
    count <- round(freq * totals, digits = 0)
    integer <- apply(
      X = count,
      MARGIN = 2,
      FUN = function(x) as.integer(round(x, digits = 0))
    )
    dimnames(integer) <- dimnames(freq)
    .CountMatrix(
      integer,
      id = from@id,
      dates = from@dates,
      coordinates = from@coordinates,
      epsg = from@epsg
    )
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
  if (isS4(from)) {
    id <- from@id
    dates <- from@dates
    coordinates <- from@coordinates
    epsg <- from@epsg
  } else {
    id <- generate_uuid()
    dates <- matrix(0, 0, 2, dimnames = list(NULL, c("value", "error")))
    coordinates <- matrix(0, 0, 3, dimnames = list(NULL, c("X", "Y", "Z")))
    epsg <- 0L
  }
  .IncidenceMatrix(
    data,
    id = id,
    dates = dates,
    coordinates = coordinates,
    epsg = epsg
  )
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
    paste0("V", seq_len(p))
  } else {
    colnames(data)
  }

  # @param indices A length-two numeric vector
  # @param data A numeric or logical matrix
  fun <- function(indices, data) {
    sum(data[, indices[1]] + data[, indices[2]] == 2)
  }
  # Get all combinations of variables, taken 2 at a time
  combine <- utils::combn(seq_len(p), 2, simplify = TRUE)
  occurrence <- apply(X = combine, MARGIN = 2, FUN = fun, data = data) / m

  C <- matrix(data = FALSE, nrow = p, ncol = p, dimnames = list(labels, labels))
  C[lower.tri(C, diag = FALSE)] <- occurrence
  C <- t(C)
  C[lower.tri(C, diag = FALSE)] <- occurrence

  id <- ifelse(isS4(from), from@id, generate_uuid())
  .OccurrenceMatrix(C, id = id)
}

setAs(from = "matrix", to = "OccurrenceMatrix", def = matrix2occurrence)
setAs(from = "data.frame", to = "OccurrenceMatrix", def = matrix2occurrence)

setAs(from = "CountMatrix", to = "OccurrenceMatrix", def = matrix2occurrence)
setAs(from = "FrequencyMatrix", to = "OccurrenceMatrix",
      def = matrix2occurrence)
setAs(from = "IncidenceMatrix", to = "OccurrenceMatrix",
      def = matrix2occurrence)
