# LogicalMatrix
#' @include AllClasses.R
NULL

## Initilize ===================================================================
# LogicalMatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
#                           dimnames = NULL) {
#   M <- as.logical(matrix(data, nrow, ncol, byrow, dimnames))
#   new("LogicalMatrix", M)
# }

#' @export
#' @rdname LogicalMatrix
IncidenceMatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  if (is.null(dimnames)) {
    dimnames <- list(1:nrow, paste("V", 1:ncol, sep = ""))
  }
  M <- matrix(as.logical(data), nrow, ncol, byrow, dimnames)
  new("IncidenceMatrix", M)
}
#' @export
#' @rdname LogicalMatrix
StratigraphyMatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  if (is.null(dimnames)) {
    dimnames <- list(1:nrow, paste("V", 1:ncol, sep = ""))
  }
  M <- matrix(as.logical(data), nrow, ncol, byrow, dimnames)
  new("StratigraphyMatrix", M)
}

## Coercions ===================================================================
setAs(from = "LogicalMatrix", to = "vector",
      def = function(from) as.vector(as(from, "matrix")))
setAs(from = "LogicalMatrix", to = "numeric",
      def = function(from) as.numeric(as(from, "matrix")))
setAs(from = "LogicalMatrix", to = "logical",
      def = function(from) as.logical(as(from, "matrix")))
setAs(from = "LogicalMatrix", to = "integer",
      def = function(from) as.integer(as(from, "matrix")))
setAs(from = "LogicalMatrix", to = "complex",
      def = function(from) as.complex(as(from, "matrix")))
setAs(from = "LogicalMatrix", to = "data.frame",
      def = function(from) as.data.frame(as(from, "matrix")))

matrix2incidence <- function(from) {
  data <- data.matrix(from)
  data <- data > 0
  object <- new("IncidenceMatrix", data)
  validObject(object)
  return(object)
}
setAs(from = "matrix", to = "IncidenceMatrix", def = matrix2incidence)
setAs(from = "data.frame", to = "IncidenceMatrix", def = matrix2incidence)

matrix2stratigraphy <- function(from) {
  data <- data.matrix(from)
  data <- apply(X = data, MARGIN = 2, FUN = as.logical)
  object <- new("StratigraphicMatrix", data)
  validObject(object)
  return(object)
}
setAs(from = "matrix", to = "StratigraphicMatrix", def = matrix2stratigraphy)
setAs(from = "data.frame", to = "StratigraphicMatrix", def = matrix2stratigraphy)

setAs(
  from = "CountMatrix",
  to = "IncidenceMatrix",
  def = function(from) {
    counts <- S3Part(from, strictS3 = TRUE, "matrix")
    binary <- counts > 0
    object <- new("IncidenceMatrix", binary)
    validObject(object)
    return(object)
  }
)
setAs(
  from = "FrequencyMatrix",
  to = "IncidenceMatrix",
  def = function(from) {
    freq <- S3Part(from, strictS3 = TRUE, "matrix")
    binary <- freq > 0
    object <- new("IncidenceMatrix", binary)
    validObject(object)
    return(object)
  }
)
