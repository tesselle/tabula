# NumericMatrix
#' @include AllClasses.R
NULL

## Initilize ===================================================================
# NumericMatrix <- function() {}

#' @export
#' @rdname NumericMatrix
CountMatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
                        dimnames = NULL) {
  if (is.null(dimnames)) {
    dimnames <- list(1:nrow, paste("V", 1:ncol, sep = ""))
  }
  M <- matrix(data, nrow, ncol, byrow, dimnames)
  new("CountMatrix", M)
}
#' @export
#' @rdname NumericMatrix
FrequencyMatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL) {
  if (is.null(dimnames)) {
    dimnames <- list(1:nrow, paste("V", 1:ncol, sep = ""))
  }
  M <- matrix(data, nrow, ncol, byrow, dimnames)
  totals <- rowSums(M)
  M <- M / totals
  new("FrequencyMatrix", M, totals = totals)
}

## Coercions ===================================================================
setAs(from = "NumericMatrix", to = "vector",
      def = function(from) as.vector(as(from, "matrix")))
setAs(from = "NumericMatrix", to = "numeric",
      def = function(from) as.numeric(as(from, "matrix")))
setAs(from = "NumericMatrix", to = "logical",
      def = function(from) as.logical(as(from, "matrix")))
setAs(from = "NumericMatrix", to = "integer",
      def = function(from) as.integer(as(from, "matrix")))
setAs(from = "NumericMatrix", to = "complex",
      def = function(from) as.complex(as(from, "matrix")))
setAs(from = "NumericMatrix", to = "data.frame",
      def = function(from) as.data.frame(as(from, "matrix")))

matrix2count <- function(from) {
  data <- data.matrix(from)
  # Work around to ensure that identical() returns TRUE (see below)
  double <- data * 1
  dimnames(double) <- dimnames(data)
  object <- new("CountMatrix", double)
  validObject(object)
  return(object)
}
setAs(from = "matrix", to = "CountMatrix", def = matrix2count)
setAs(from = "data.frame", to = "CountMatrix", def = matrix2count)

matrix2frequency <- function(from) {
  data <- data.matrix(from)
  totals <- rowSums(data)
  freq <- data / totals
  dimnames(freq) <- dimnames(data)
  object <- new("FrequencyMatrix", freq, totals = totals)
  validObject(object)
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
    counts <- S3Part(from, strictS3 = TRUE, "matrix")
    totals <- rowSums(counts)
    freq <- counts / totals
    object <- new("FrequencyMatrix", freq, totals = totals)
    validObject(object)
    return(object)
  }
)
setAs(
  from = "FrequencyMatrix",
  to = "CountMatrix",
  def = function(from) {
    freq <- S3Part(from, strictS3 = TRUE, "matrix")
    totals <- from@totals
    count <- freq * totals
    object <- new("CountMatrix", count)
    validObject(object)
    return(object)
  }
)
