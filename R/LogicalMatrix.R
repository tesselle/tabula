#' @include AllClasses.R
NULL

## Create ======================================================================
#' @export
#' @rdname IncidenceMatrix-class
IncidenceMatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  data <- as.logical(data)
  M <- buildMatrix(data, nrow, ncol, byrow, dimnames,
                   missing(nrow), missing(ncol))
  methods::new("IncidenceMatrix", M)
}

# Coerce =======================================================================
## From LogicalMatrix ----------------------------------------------------------
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
  data <- if (isS4(from)) methods::as(from, "matrix")
          else data.matrix(from)
  data <- data > 0
  p <- ncol(data)
  labels <- if (is.null(colnames(data))) paste("V", 1:p, sep = "")
            else colnames(data)

  # @param indices A length-two numeric vector
  # @param data A numeric or logical matrix
  fun <- function(indices, data) {
    any(data[, indices[1]] + data[, indices[2]] == 2)
  }
  # Get all combinations of variables, taken 2 at a time
  combine <- utils::combn(1:p, 2, simplify = TRUE)
  occurrence <- apply(X = combine, MARGIN = 2, FUN = fun, data = data)

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
