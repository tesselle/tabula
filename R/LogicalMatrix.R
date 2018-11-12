# LogicalMatrix
#' @include AllClasses.R
NULL

## Initilize ===================================================================
# LogicalMatrix <- function() {}

#' @export
#' @rdname LogicalMatrix
IncidenceMatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  data <- as.logical(data)
  M <- buildMatrix(data, nrow, ncol, byrow, dimnames,
                   missing(nrow), missing(ncol))
  methods::new("IncidenceMatrix", M)
}
# @export
# @rdname LogicalMatrix
# StratigraphicMatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
#                                 dimnames = NULL) {
#   data <- as.logical(data)
#   M <- buildMatrix(data, nrow, ncol, byrow, dimnames,
#                    missing(nrow), missing(ncol))
#   methods::new("StratigraphicMatrix", M)
# }

## Coercions ===================================================================
setAs(from = "LogicalMatrix", to = "vector",
      def = function(from) as.vector(methods::as(from, "matrix")))
setAs(from = "LogicalMatrix", to = "numeric",
      def = function(from) as.numeric(methods::as(from, "matrix")))
setAs(from = "LogicalMatrix", to = "logical",
      def = function(from) as.logical(methods::as(from, "matrix")))
setAs(from = "LogicalMatrix", to = "integer",
      def = function(from) as.integer(methods::as(from, "matrix")))
setAs(from = "LogicalMatrix", to = "complex",
      def = function(from) as.complex(methods::as(from, "matrix")))
setAs(from = "LogicalMatrix", to = "data.frame",
      def = function(from) as.data.frame(methods::as(from, "matrix")))

matrix2incidence <- function(from) {
  data <- data.matrix(from)
  data <- data > 0
  object <- methods::new("IncidenceMatrix", data)
  methods::validObject(object)
  return(object)
}
setAs(from = "matrix", to = "IncidenceMatrix", def = matrix2incidence)
setAs(from = "data.frame", to = "IncidenceMatrix", def = matrix2incidence)

matrix2stratigraphy <- function(from) {
  data <- data.matrix(from)
  data <- apply(X = data, MARGIN = 2, FUN = as.logical)
  object <- methods::new("StratigraphicMatrix", data)
  methods::validObject(object)
  return(object)
}
setAs(from = "matrix", to = "StratigraphicMatrix", def = matrix2stratigraphy)
setAs(from = "data.frame", to = "StratigraphicMatrix", def = matrix2stratigraphy)

setAs(
  from = "CountMatrix",
  to = "IncidenceMatrix",
  def = function(from) {
    counts <- methods::S3Part(from, strictS3 = TRUE, "matrix")
    binary <- counts > 0
    object <- methods::new("IncidenceMatrix", binary)
    methods::validObject(object)
    return(object)
  }
)
setAs(
  from = "FrequencyMatrix",
  to = "IncidenceMatrix",
  def = function(from) {
    freq <- methods::S3Part(from, strictS3 = TRUE, "matrix")
    binary <- freq > 0
    object <- methods::new("IncidenceMatrix", binary)
    methods::validObject(object)
    return(object)
  }
)
