#' @include AllClasses.R
NULL

# Create =======================================================================
#' @export
#' @rdname CountMatrix-class
CountMatrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
                        dimnames = NULL) {
  M <- buildMatrix(as.integer(data), nrow, ncol, byrow, dimnames,
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

# Coerce =======================================================================
## From NumericMatrix ----------------------------------------------------------
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
