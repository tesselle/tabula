# NumericMatrix
#' @include AllClasses.R
NULL

## Initilize ===================================================================
# NumericMatrix <- function() {}

#' @export
#' @rdname NumericMatrix
CountMatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
                        dimnames = NULL) {
  M <- buildMatrix(data, nrow, ncol, byrow, dimnames,
                   missing(nrow), missing(ncol))
  methods::new("CountMatrix", M)
}

# @export
# @rdname NumericMatrix
# FrequencyMatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
#                             dimnames = NULL) {
#   M <- buildMatrix(data, nrow, ncol, byrow, dimnames,
#                    missing(nrow), missing(ncol))
#   totals <- rowSums(M)
#   M <- M / totals
#   methods::new("FrequencyMatrix", M, totals = totals)
# }

## Coercions ===================================================================
setAs(from = "NumericMatrix", to = "vector",
      def = function(from) as.vector(methods::as(from, "matrix")))
setAs(from = "NumericMatrix", to = "numeric",
      def = function(from) as.numeric(methods::as(from, "matrix")))
setAs(from = "NumericMatrix", to = "logical",
      def = function(from) as.logical(methods::as(from, "matrix")))
setAs(from = "NumericMatrix", to = "integer",
      def = function(from) as.integer(methods::as(from, "matrix")))
setAs(from = "NumericMatrix", to = "complex",
      def = function(from) as.complex(methods::as(from, "matrix")))
setAs(from = "NumericMatrix", to = "data.frame",
      def = function(from) as.data.frame(methods::as(from, "matrix")))

matrix2count <- function(from) {
  data <- data.matrix(from)
  # Work around to ensure that identical() returns TRUE (see below)
  double <- data * 1
  dimnames(double) <- dimnames(data)
  object <- methods::new("CountMatrix", double)
  methods::validObject(object)
  return(object)
}
setAs(from = "matrix", to = "CountMatrix", def = matrix2count)
setAs(from = "data.frame", to = "CountMatrix", def = matrix2count)

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

# When coercing a CountMatrix to a FrequencyMatrix then to a CountMatrix again,
# identical() returns FALSE, unless the starting CountMatrix is coerced to
# double() and the final one is rounded (no decimal place).
# NumericMatrix
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
    # Work around to ensure that identical() returns TRUE (see below)
    count <- round(freq * totals, digits = 0)
    object <- methods::new("CountMatrix", count)
    methods::validObject(object)
    return(object)
  }
)
