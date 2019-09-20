# CLASSES DEFINITION AND INITIALIZATION
#' @include utilities.R
NULL

# DEFINITION ===================================================================
## -----------------------------------------------------------------------------
#' Date model
#'
#' An S4 class to store the event and accumulation times of archaeological
#'  assemblages.
#' @slot counts A numeric matrix of count data.
#' @slot level A length-one \code{\link{numeric}} vector giving the
#'  confidence level.
#' @slot model A \code{\link[stats:lm]{multiple linear model}}: the Gaussian
#'  multiple linear regression model fitted for event date estimation and
#'  prediction.
#' @slot rows A four columns \code{\link{numeric}} matrix giving the predicted
#'  event dates for each archaeological assemblage, with the following columns:
#'  \describe{
#'   \item{date}{The event date estimation.}
#'   \item{lower}{The lower boundary of the confidence interval.}
#'   \item{upper}{The upper boundary of the confidence interval.}
#'   \item{error}{The standard error of predicted dates.}
#'  }
#' @slot columns A four columns \code{\link{numeric}} matrix giving the
#'  predicted event dates for each archaeological type or fabric, with the
#'  following columns:
#'  \describe{
#'   \item{date}{The event date estimation.}
#'   \item{lower}{The lower boundary of the confidence interval.}
#'   \item{upper}{The upper boundary of the confidence interval.}
#'   \item{error}{The standard error of predicted dates.}
#'  }
#' @slot accumulation A two columns \code{\link{numeric}} matrix giving the
#'  point estimate of the accumulation dates and the corresponding error.
#' @section Subset:
#'  \describe{
#'   \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'   subscript \code{i}. \code{i} is a length-one \code{\link{character}}
#'   vector. Returns the corresponding slot values.}
#'  }
#' @author N. Frerebeau
#' @docType class
#' @aliases DateModel-class
.DateModel <- setClass(
  Class = "DateModel",
  slots = c(
    id = "character",
    counts = "matrix",
    level = "numeric",
    model = "lm",
    rows = "matrix",
    columns = "matrix",
    accumulation = "matrix"
  ),
  prototype = list(
    id = "00000000-0000-4000-a000-000000000000",
    counts = matrix(0, 0, 0),
    level = numeric(1),
    model = stats::lm(0 ~ 0),
    rows = matrix(
      0, 0, 4,
      dimnames = list(NULL, c("date", "lower", "upper", "error"))
    ),
    columns = matrix(
      0, 0, 4,
      dimnames = list(NULL, c("date", "lower", "upper", "error"))
    ),
    accumulation = matrix(0, 0, 2, dimnames = list(NULL, c("date", "error")))
  )
)

## -----------------------------------------------------------------------------
#' Partial bootstrap CA
#'
#' An S4 class to store partial bootstrap correspondence analysis results.
#' @slot id A \code{\link{character}} string specifying the unique
#'  identifier of the corresponding matrix (UUID v4).
#' @slot rows A list of length three giving the vertices coordinates
#'  (\code{x}, \code{y}) of the samples convex hull and a identifier
#'  (\code{id}) to link each row to a sample.
#' @slot columns A list of length three giving the vertices coordinates
#'  (\code{x}, \code{y}) of the variables convex hull and a identifier
#'  (\code{id}) to link each row to a variable.
#' @slot lengths A list of two named \code{\link{numeric}} vectors giving the
#'  convex hull maximum dimension length of samples and variables, respectively.
#' @slot cutoff A length-two \code{\link{numeric}} vector giving the cutoff
#'  value for samples and variables selection, respectively.
#' @slot keep A list of two \code{\link{integer}} vectors giving the subscript
#'  of the samples and variables to be kept, respectively.
#' @section Subset:
#'  In the code snippets below, \code{x} is a \code{BootCA} object.
#'  \describe{
#'   \item{\code{x[i, j, drop]}}{Extracts informations from a slot selected by
#'   subscript \code{i} thru \code{j} (see examples). \code{i} should be one of
#'   "\code{rows}" or "\code{columns}". Any unambiguous substring can be
#'   given. \code{j} is a \code{\link{numeric}}, \code{\link{integer}} or
#'   \code{\link{character}} vector or empty (missing) or \code{NULL}.
#'   Numeric values are coerced to \code{\link{integer}} as by
#'   \code{\link{as.integer}} (and hence truncated towards zero). Character
#'   vectors will be matched to the name of the elements. An empty index
#'   (a comma separated blank) indicates that all entries in that dimension are
#'   selected.}
#'   \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'   subscript \code{i}. \code{i} should be one of "\code{id}",
#'   "\code{rows}", "\code{columns}", "\code{lengths}", "\code{cutoff}" or
#'   "\code{keep}". Any unambiguous substring can be given.}
#'  }
#' @author N. Frerebeau
#' @docType class
#' @aliases BootCA-class
.BootCA <- setClass(
  Class = "BootCA",
  slots = c(
    id = "character",
    rows = "list",
    columns = "list",
    lengths = "list",
    cutoff = "numeric",
    keep = "list"
  ),
  prototype = list(
    id = "00000000-0000-4000-a000-000000000000",
    rows = list(id = factor(), x = numeric(0), y = numeric(0)),
    columns = list(id = factor(), x = numeric(0), y = numeric(0)),
    lengths = list(numeric(0), numeric(0)),
    cutoff = numeric(0),
    keep = list(integer(0), integer(0))
  )
)

## -----------------------------------------------------------------------------
#' Permutation order
#'
#' An S4 class to represent a permutation order.
#' @slot id A \code{\link{character}} string specifying the unique
#'  identifier of the corresponding matrix (UUID v4).
#' @slot rows An \code{\link{integer}} vector giving the rows permutation.
#' @slot columns An \code{\link{integer}} vector giving the columns permutation.
#' @slot method A \code{\link{character}} string indicating the seriation
#'  method used.
#' @section Subset:
#'  \describe{
#'   \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'   subscript \code{i}. \code{i} should be one of "\code{id}",
#'   "\code{rows}", "\code{columns}" or "\code{method}". Any unambiguous
#'   substring can be given.}
#'  }
#' @note
#'  Numeric values are coerced to \code{\link{integer}} as by
#'  \code{\link[base]{as.integer}} (and hence truncated towards zero).
#' @author N. Frerebeau
#' @docType class
#' @aliases PermutationOrder-class
.PermutationOrder <- setClass(
  Class = "PermutationOrder",
  slots = c(
    id = "character",
    rows = "integer",
    columns = "integer",
    method = "character"
  ),
  prototype = list(
    id = "00000000-0000-4000-a000-000000000000",
    rows = integer(0),
    columns = integer(0),
    method = "unknown"
  )
)

## -----------------------------------------------------------------------------
#' Space and Time
#'
#' An S4 class to represent space-time informations.
#' @slot dates A two column \code{\link{numeric}} matrix giving
#'  the date \code{value} and \code{error}, respectively.
#' @slot coordinates A three columns \code{\link{numeric}} matrix
#'  (\code{x}, \code{y} and \code{z}) giving the geographic coordinates
#'  (longitude, latitude and elevation, respectively).
#' @slot epsg An \code{\link{integer}} giving the EPSG code of the spatial
#'  reference system used. Numeric values are coerced to \code{\link{integer}}
#'  as by \code{\link{as.integer}} (and hence truncated towards zero).
#' @section Get and set:
#'  In the code snippets below, \code{x} is a \code{SpaceTime} object.
#'  \describe{
#'   \item{\code{get_dates(x)}, \code{set_dates(x) <- value}}{Get or set the dates
#'   of \code{x} according to \code{value}
#'   (see \code{\link[=set_dates<-]{set_dates}} for details).}
#'   \item{\code{get_coordinates(x)}, \code{set_coordinates(x) <- value}}{Get or
#'   set the geographical coordinates of \code{x} according to \code{value}
#'   (see \code{\link[=set_coordinates<-]{set_coordinates}} for details).}
#'   \item{\code{get_epsg(x)}, \code{set_epsg(x) <- value}}{Get or
#'   set the EPSG of \code{x} according to \code{value}. Numeric values are
#'   coerced to \code{\link{integer}} as by \code{\link{as.integer}} (and hence
#'   truncated towards zero).}
#'   \item{\code{get_features(x)}}{Convert an \code{AbundanceMatrix}
#'   object to a \code{\link[=data.frame]{data frame}} with dates and
#'   coordinates columns.}
#'  }
#' @author N. Frerebeau
#' @docType class
#' @aliases SpaceTime-class
#' @keywords internal
.SpaceTime <- setClass(
  Class = "SpaceTime",
  slots = c(
    dates = "matrix",
    coordinates = "matrix",
    epsg = "integer"
  ),
  prototype = list(
    dates = matrix(0, 0, 2, dimnames = list(NULL, c("value", "error"))),
    coordinates = matrix(0, 0, 3, dimnames = list(NULL, c("X", "Y", "Z"))),
    epsg = as.integer(0)
  )
)

## Matrix ----------------------------------------------------------------------
#' Matrix
#'
#' An S4 class to represent a matrix. This class extends the \code{base}
#' \link[base]{matrix}.
#' @slot id A \code{\link{character}} string specifying the unique
#'  identifier of the matrix (UUID v4).
#' @section Matrix ID:
#'  When a matrix is first created, an identifier is generated (UUID v4).
#'  This ID is preserved when coercing to another class. Thus, the object ID is
#'  unique within the same class, but two objects of different classes can have
#'  the same ID. This makes it possible to identify objects representing the
#'  same initial data and associate them with the results of specific
#'  computations (e. g. \link[=seriate]{seriation}).
#' @section Get and set:
#'  In the code snippets below, \code{x} is a \code{*Matrix} object.
#'  \describe{
#'   \item{\code{get_id(x)}}{Get the ID of \code{x}.}
#'  }
#' @section Access:
#'  In the code snippets below, \code{x} is a \code{*Matrix} object.
#'  \describe{
#'   \item{\code{dim(x)}}{Returns the dimension of \code{x}.}
#'   \item{\code{nrow(x)}}{Returns the number of rows present in \code{x}.}
#'   \item{\code{ncol(x)}}{Returns the number of columns present in \code{x}.}
#'   \item{\code{dimnames(x)}, \code{dimnames(x) <- value}}{Retrieves or sets
#'   the row dimnames of \code{x} according to \code{value}.}
#'   \item{\code{rownames(x)}, \code{rownames(x) <- value}}{Retrieves or sets
#'   the row names of \code{x} according to \code{value}.}
#'   \item{\code{colnames(x)}, \code{colnames(x) <- value}}{Retrieves or sets
#'   the column names of \code{x} according to \code{value}.}
#'  }
#' @section Subset:
#'  In the code snippets below, \code{x} is a \code{*Matrix} object.
#'  \describe{
#'   \item{\code{x[i, j]}}{Extracts elements selected by subscripts \code{i}
#'   and \code{j}. Indices are \code{\link{numeric}}, \code{\link{integer}} or
#'   \code{\link{character}} vectors or empty (missing) or \code{NULL}.
#'   Numeric values are coerced to \code{\link{integer}} as by
#'   \code{\link{as.integer}} (and hence truncated towards zero).
#'   Character vectors will be matched to the name of the elements.
#'   An empty index (a comma separated blank) indicates that all
#'   entries in that dimension are selected.
#'   Returns an object of the same class as \code{x}.}
#'   \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'   subscript \code{i}. \code{i} should be one of "\code{id}" or \code{NULL}.}
#'  }
#' @seealso \link[base]{matrix}
#' @author N. Frerebeau
#' @family matrix class
#' @docType class
#' @aliases Matrix-class
#' @keywords internal
.Matrix <- setClass(
  Class = "Matrix",
  slots = c(
    id = "character"
  ),
  prototype = prototype(
    matrix(0, 0, 0),
    id = "00000000-0000-4000-a000-000000000000"
  ),
  contains = "matrix"
)

## Numeric matrix --------------------------------------------------------------
#' Numeric matrix
#'
#' An S4 class to represent a numeric matrix.
#' @inheritSection Matrix-class Matrix ID
#' @inheritSection Matrix-class Get and set
#' @inheritSection Matrix-class Access
#' @inheritSection Matrix-class Subset
#' @seealso \linkS4class{Matrix}
#' @author N. Frerebeau
#' @family matrix class
#' @docType class
#' @aliases NumericMatrix-class
#' @keywords internal
.NumericMatrix <- setClass(
  Class = "NumericMatrix",
  contains = "Matrix"
)

#' Count matrix
#'
#' An S4 class to represent a count matrix.
#' @inheritParams base::matrix
#' @inheritSection Matrix-class Matrix ID
#' @inheritSection Matrix-class Get and set
#' @inheritSection Matrix-class Access
#' @inheritSection Matrix-class Subset
#' @return
#'  TODO
#' @note
#'  Numeric values are \code{\link[base:round]{rounded}} to zero decimal places
#'  and then coerced to \code{\link{integer}} as by
#'  \code{\link[base]{as.integer}}.
#' @seealso \linkS4class{NumericMatrix}, \linkS4class{SpaceTime}
#' @family abundance matrix
#' @example inst/examples/ex-abundance-class.R
#' @author N. Frerebeau
#' @family matrix class
#' @docType class
#' @aliases CountMatrix-class
.CountMatrix <- setClass(
  Class = "CountMatrix",
  contains = c("NumericMatrix", "SpaceTime")
)

#' Frequency matrix
#'
#' An S4 class to represent a relative frequency matrix.
#' @slot total A \code{\link{numeric}} vector.
#' @details
#'  To ensure data integrity, a \code{FrequencyMatrix} can only be created by
#'  coercion from a \linkS4class{CountMatrix} (see examples).
#' @inheritSection Matrix-class Matrix ID
#' @section Get and set:
#'  In the code snippets below, \code{x} is a \code{FrequencyMatrix} object.
#'  \describe{
#'   \item{\code{get_id(x)}}{Get the unique ID of \code{x}.}
#'   \item{\code{get_totals(x)}}{Get the row sums (counts) of \code{x}.}
#'  }
#' @inheritSection Matrix-class Access
#' @inheritSection Matrix-class Subset
#' @seealso \linkS4class{NumericMatrix}, \linkS4class{SpaceTime}
#' @family abundance matrix
#' @example inst/examples/ex-abundance-class.R
#' @author N. Frerebeau
#' @family matrix class
#' @docType class
#' @aliases FrequencyMatrix-class
.FrequencyMatrix <- setClass(
  Class = "FrequencyMatrix",
  slots = c(
    totals = "numeric"
  ),
  prototype = prototype(
    matrix(0, 0, 0),
    totals = numeric(0)
  ),
  contains = c("NumericMatrix", "SpaceTime")
)

#' Co-occurrence matrix
#'
#' An S4 class to represent a co-occurrence matrix.
#' @details
#'  A co-occurrence matrix is a symmetric matrix with zeros on its main diagonal,
#'  which works out how many times (expressed in percent) each pairs of
#'  taxa/types occur together in at least one sample.
#' @inheritSection Matrix-class Matrix ID
#' @inheritSection Matrix-class Get and set
#' @inheritSection Matrix-class Access
#' @inheritSection Matrix-class Subset
#' @seealso \linkS4class{NumericMatrix}
#' @family abundance matrix
#' @example inst/examples/ex-abundance-class.R
#' @author N. Frerebeau
#' @family matrix class
#' @docType class
#' @aliases OccurrenceMatrix-class
.OccurrenceMatrix <- setClass(
  Class = "OccurrenceMatrix",
  contains = "NumericMatrix"
)

#' Similarity matrix
#'
#' An S4 class to represent a (dis)similarity matrix.
#' @slot method A \code{\link{character}} string specifying the distance
#'  method used.
#' @inheritSection Matrix-class Matrix ID
#' @inheritSection Matrix-class Get and set
#' @inheritSection Matrix-class Access
#' @inheritSection Matrix-class Subset
#' @seealso \linkS4class{NumericMatrix}
#' @family matrix class
#' @author N. Frerebeau
#' @docType class
#' @aliases SimilarityMatrix-class
.SimilarityMatrix <- setClass(
  Class = "SimilarityMatrix",
  slots = c(
    method = "character"
  ),
  prototype = prototype(
    matrix(0, 0, 0),
    method = "unknown"
  ),
  contains = "NumericMatrix"
)

## Logical matrix --------------------------------------------------------------
#' Logical matrix
#'
#' An S4 class to represent a logical matrix.
#' @inheritSection Matrix-class Matrix ID
#' @inheritSection Matrix-class Get and set
#' @inheritSection Matrix-class Access
#' @inheritSection Matrix-class Subset
#' @seealso \linkS4class{Matrix}
#' @author N. Frerebeau
#' @family matrix class
#' @docType class
#' @aliases LogicalMatrix-class
#' @keywords internal
.LogicalMatrix <- setClass(
  Class = "LogicalMatrix",
  contains = "Matrix"
)

#' Incidence matrix
#'
#' An S4 class to represent an incidence (presence/absence) matrix.
#' @inheritParams base::matrix
#' @details
#'  Numeric values are coerced to \code{\link{logical}} as by
#'  \code{\link[base]{as.logical}}.
#' @inheritSection Matrix-class Matrix ID
#' @inheritSection Matrix-class Get and set
#' @inheritSection Matrix-class Access
#' @inheritSection Matrix-class Subset
#' @return
#'  TODO
#' @seealso \linkS4class{LogicalMatrix}, \linkS4class{SpaceTime}
#' @family logical matrix
#' @example inst/examples/ex-logical-class.R
#' @author N. Frerebeau
#' @family matrix class
#' @docType class
#' @aliases IncidenceMatrix-class
.IncidenceMatrix <- setClass(
  Class = "IncidenceMatrix",
  contains = c("LogicalMatrix", "SpaceTime")
)

## Abundance matrix ------------------------------------------------------------
setClassUnion(
  name = "AbundanceMatrix",
  members = c("CountMatrix", "FrequencyMatrix", "IncidenceMatrix")
)

# INITIALIZATION ===============================================================
## DateModel -------------------------------------------------------------------
DateModel <- function(
  id = generate_uuid(), counts = matrix(0, 0, 0),
  level = numeric(1), model = stats::lm(0 ~ 0),
  rows = matrix(0, 0, 4,
                dimnames = list(NULL, c("date", "lower", "upper", "error"))),
  columns = matrix(0, 0, 4,
                   dimnames = list(NULL, c("date", "lower", "upper", "error"))),
  accumulation = matrix(0, 0, 2, dimnames = list(NULL, c("date", "error")))
) {
  throw_message_class("SimilarityMatrix")

  .DateModel(
    id = id,
    counts = counts,
    level = level,
    model = model,
    rows = rows,
    columns = columns,
    accumulation = accumulation
  )
}
## BootCA ----------------------------------------------------------------------
BootCA <- function(
  id = generate_uuid(),
  rows = list(id = factor(), x = numeric(0), y = numeric(0)),
  columns = list(id = factor(), x = numeric(0), y = numeric(0)),
  lengths = list(numeric(0), numeric(0)),
  cutoff = c(0, 0), keep = list(integer(0), integer(0))
) {
  throw_message_class("BootCA")

  rows <- mapply(
    FUN = function(x, type) type(x),
    rows, list(as.factor, as.numeric, as.numeric),
    SIMPLIFY = FALSE
  )
  columns <- mapply(
    FUN = function(x, type) type(x),
    columns, list(as.factor, as.numeric, as.numeric),
    SIMPLIFY = FALSE
  )
  lengths <- lapply(X = lengths, FUN = as.numeric)
  lengths <- mapply(FUN = `names<-`,
                    lengths, list(unique(rows$id), unique(columns$id)),
                    SIMPLIFY = FALSE)
  # keep <- lapply(X = keep, FUN = as.integer)
  .BootCA(
    id = id,
    rows = rows,
    columns = columns,
    lengths = lengths,
    cutoff = as.numeric(cutoff),
    keep = keep
  )
}
## PermutationOrder ------------------------------------------------------------
PermutationOrder <- function(id = generate_uuid(), rows = integer(0),
                             columns = integer(0), method = "unknown") {
  throw_message_class("PermutationOrder")
  .PermutationOrder(
    id = as.character(id),
    rows = as.integer(rows),
    columns = as.integer(columns),
    method = as.character(method)
  )
}
## SpaceTime -------------------------------------------------------------------
SpaceTime <- function(
  dates = matrix(0, 0, 2, dimnames = list(NULL, c("value", "error"))),
  coordinates = matrix(0, 0, 3, dimnames = list(NULL, c("X", "Y", "Z"))),
  epsg = 0, ...
) {
  throw_message_class("SpaceTime")
  colnames(coordinates) <- toupper(colnames(coordinates))
  .SpaceTime(
    dates = dates,
    coordinates = coordinates,
    epsg = as.integer(epsg),
    ...
  )
}
## *Matrix ---------------------------------------------------------------------
Matrix <- function(...) {
  throw_message_class("Matrix")
  .Matrix(..., id = generate_uuid(seed = NULL))
}
NumericMatrix <- function(data = matrix(0, 0, 0), ...) {
  throw_message_class("NumericMatrix")
  .NumericMatrix(Matrix(data), ...)
}
LogicalMatrix <- function(data = matrix(FALSE, 0, 0), ...) {
  throw_message_class("LogicalMatrix")
  .LogicalMatrix(Matrix(data), ...)
}
OccurrenceMatrix <- function(data = matrix(0, 0, 0), ...) {
  throw_message_class("OccurrenceMatrix")
  .OccurrenceMatrix(NumericMatrix(data), ...)
}
SimilarityMatrix <- function(data = matrix(0, 0, 0), method = "unknown", ...) {
  throw_message_class("SimilarityMatrix")
  .SimilarityMatrix(NumericMatrix(data), method = as.character(method), ...)
}

#' @export
#' @rdname CountMatrix-class
CountMatrix <- function(data = 0, nrow = 1, ncol = 1, byrow = FALSE,
                        dimnames = NULL, ...) {
  throw_message_class("CountMatrix")
  M <- buildMatrix(data, nrow, ncol, byrow, dimnames,
                   missing(nrow), missing(ncol))
  .CountMatrix(NumericMatrix(M), ...)
}

#' @export
#' @rdname IncidenceMatrix-class
IncidenceMatrix <- function(data = FALSE, nrow = 1, ncol = 1, byrow = FALSE,
                            dimnames = NULL, ...) {
  throw_message_class("IncidenceMatrix")
  data <- as.logical(data)
  M <- buildMatrix(data, nrow, ncol, byrow, dimnames,
                   missing(nrow), missing(ncol))
  .IncidenceMatrix(LogicalMatrix(M), ...)
}

# CREATE =======================================================================
#' Matrix constructor
#'
#' @inheritParams base::matrix
#' @param rows A \code{\link{logical}} scalar: is the number of rows
#'  unspecified?
#' @param cols A \code{\link{logical}} scalar: is the number of columns
#'  unspecified?
#' @return A \link{\code{matrix}}.
#' @author N. Frerebeau
#' @name matrix-constructors
#' @keywords internal
#' @noRd
buildMatrix <- function(data, nrow, ncol, byrow, dimnames,
                        rows = FALSE, cols = FALSE) {
  k <- length(data)
  if (rows) nrow <- k / ncol
  if (cols) ncol <- k / nrow
  if (is.null(dimnames)) {
    dimnames <- list(seq_len(nrow), paste("V", seq_len(ncol), sep = ""))
  } else {
    if (is.null(dimnames[[1]])) dimnames[[1]] <- seq_len(nrow)
    if (is.null(dimnames[[2]])) dimnames[[2]] <- paste0("V", seq_len(ncol))
  }
  M <- matrix(data, nrow, ncol, byrow, dimnames)
  return(M)
}
