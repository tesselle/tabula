# SERIATION METHODS

# Seriation methods
seriation <- function(object, method = c("correspondence", "reciprocal"),
                      EPPM = FALSE, ...) {
  # Validation
  method <- match.arg(method, several.ok = FALSE)
  data <- if (EPPM) independance(object, method = "EPPM") else object

  index <- switch(
    method,
    reciprocal = seriationReciprocal(data, ...),
    correspondence = seriationCorrespondence(data, ...),
    stop(sprintf("There is no such method: %s.", method), call. = FALSE)
  )

  # New PermutationOrder object
  .PermutationOrder(
    id = arkhe::get_id(object),
    rows = as.integer(index[[1]]),
    columns = as.integer(index[[2]]),
    method = method
  )
}

# ==============================================================================
#' Probabilistic seriation methods
#'
#' \code{seriationReciprocal} computes reciprocal ranking.
#' \code{seriationCorrespondence} computes CA-based seriation.
#' @param x A \code{\link{numeric}} matrix.
#' @param stop A length-one \code{\link{numeric}} vector giving the stopping rule
#'  (i.e. maximum number of iterations) to avoid infinite loop.
#' @param margin A \code{\link{numeric}} vector giving the subscripts which the
#'  rearrangement will be applied over. E.g., for a matrix \code{1} indicates
#'  rows, \code{2} indicates columns, \code{c(1, 2)} indicates rows then columns,
#'  \code{c(2, 1)} indicates columns then rows.
#' @param ... Further arguments to be passed to \code{\link[ca]{ca}}.
#' @return A list of two \code{\link{numeric}} vectors.
#' @author N. Frerebeau
#' @family seriation methods
#' @name seriation-probabilistic
#' @keywords internal
#' @noRd

seriationReciprocal <- function(x, margin = 1, stop = 100) {
  # Validation
  margin <- as.integer(margin)
  stop <- as.integer(stop)

  # Compute ranks
  # margin = 1 : on rows
  # margin = 2 : on columns
  reorder <- function(x, margin) {
    i <- seq_len(nrow(x))
    j <- seq_len(ncol(x))
    k <- switch(
      margin,
      `1` = colSums(t(x) * j) / rowSums(x),
      `2` = colSums(x * i) / colSums(x),
      stop("`margin` subscript out of bounds.", call. = FALSE)
    )
    order(k)
  }

  start <- 0
  index <- list(rows = seq_len(nrow(x)), columns = seq_len(ncol(x)))
  convergence <- FALSE
  while (!convergence) {
    old_index <- index
    # Rearrange along margins
    for (k in margin) {
      index[[k]] <- index[[k]][reorder(x[index[[1]], index[[2]]], margin = k)]
    }
    # Loop counter
    convergence <- identical(index, old_index)
    start <- start + 1
    if (start >= stop) {
      warning("Convergence not reached (possible infinite cycle).",
              call. = FALSE)
      break
    }
  }

  index
}

seriationCorrespondence <- function(x, margin, axes = 1,
                                    verbose = getOption("verbose"), ...) {
  # Validation
  margin <- as.integer(margin)
  axes <- as.integer(axes)[[1L]]

  # /!\ Important: we need to clean the data before processing
  # Empty rows/columns must be removed to avoid error in svd()
  empty_rows <- rowSums(x) == 0
  empty_cols <- colSums(x) == 0
  x_clean <- x[!empty_rows, !empty_cols]

  if (sum(empty_rows) != 0 || sum(empty_cols) != 0) {
    row_names <- paste0(rownames(x)[empty_rows], collapse = ", ")
    col_names <- paste0(colnames(x)[empty_cols], collapse = ", ")
    msg <- "Empty values were removed:"
    if (sum(empty_rows) != 0) msg <- paste0(msg, "\n* Rows: ", row_names)
    if (sum(empty_cols) != 0) msg <- paste0(msg, "\n* Columns: ", col_names)
    warning(msg, call. = FALSE)
  }

  # Original sequences
  i <- seq_len(nrow(x_clean))
  j <- seq_len(ncol(x_clean))
  # Correspondence analysis
  corresp <- ca::ca(x_clean, ...)
  # Sequence of the first axis as best seriation order
  coords <- ca::cacoord(corresp, type = "principal")
  row_coords <- if (1 %in% margin) order(coords$rows[, axes]) else i
  col_coords <- if (2 %in% margin) order(coords$columns[, axes]) else j

  list(rows = row_coords, columns = col_coords)
}
