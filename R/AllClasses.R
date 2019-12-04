# CLASSES DEFINITION AND INITIALIZATION
#' @include utilities.R
NULL

# DEFINITION ===================================================================
#' Diversity Index
#'
#' An S4 class to represent a diversity measure.
#' @slot id A \code{\link{character}} string specifying the unique
#'  identifier of the corresponding matrix (UUID v4).
#' @slot index A \code{\link{numeric}} vector giving the diversity index values.
#' @slot size A \code{\link{numeric}} vector giving the sample sizes.
#' @slot jackknife A numeric \code{\link{matrix}} vector giving the jackknifed
#'  estimates.
#' @slot boostrap A numeric \code{\link{matrix}} vector giving the boostraped
#'  estimates.
#' @slot simulated A numeric \code{\link{matrix}} vector giving the diversity
#'  measures for the simulated assemblage.
#' @slot method A \code{\link{character}} string indicating the method used.
#' @section Subset:
#'  In the code snippets below, \code{x} is a \code{DiversityIndex} object.
#'  \describe{
#'   \item{\code{x[[i]]}}{Extracts informations from a slot selected by
#'   subscript \code{i}. \code{i} is a length-one \code{\link{character}}
#'   vector. Returns the corresponding slot values.}
#'  }
#' @author N. Frerebeau
#' @family class
#' @docType class
#' @name DiversityIndex
#' @rdname DiversityIndex
NULL

#' @rdname DiversityIndex
#' @aliases DiversityIndex-class
.DiversityIndex <- setClass(
  Class = "DiversityIndex",
  slots = c(
    id = "character",
    index = "numeric",
    size = "numeric",
    jackknife = "matrix",
    boostrap = "matrix",
    simulated = "matrix",
    method = "character"
  )
)
#' @rdname DiversityIndex
#' @aliases HeterogeneityIndex HeterogeneityIndex-class
.HeterogeneityIndex <- setClass(
  Class = "HeterogeneityIndex",
  contains = "DiversityIndex"
)
#' @rdname DiversityIndex
#' @aliases EvennessIndex EvennessIndex-class
.EvennessIndex <- setClass(
  Class = "EvennessIndex",
  contains = "DiversityIndex"
)
#' @rdname DiversityIndex
#' @aliases RichnessIndex RichnessIndex-class
.RichnessIndex <- setClass(
  Class = "RichnessIndex",
  contains = "DiversityIndex"
)

## -----------------------------------------------------------------------------
#' Date Model
#'
#' An S4 class to store the event and accumulation times of archaeological
#'  assemblages.
#' @slot id A \code{\link{character}} string specifying the unique
#'  identifier of the corresponding matrix (UUID v4).
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
#' @family class
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
#' @family class
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
#' @family class
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
