# CORRESPONDENCE ANALYSIS
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname ca
#' @aliases run_ca,CountMatrix-method
setMethod(
  f = "run_ca",
  signature = signature(object = "CountMatrix"),
  definition = function(object, dimensions = NULL, ...) {
    data <- as.matrix(object)
    # /!\ Important: we need to clean the data before processing
    # Empty rows/columns must be removed to avoid error in svd()
    empty_rows <- rowSums(data) == 0
    empty_cols <- colSums(data) == 0
    data_clean <- data[!empty_rows, !empty_cols]

    if (sum(empty_rows) != 0 || sum(empty_cols) != 0) {
      row_names <- paste0(rownames(data)[empty_rows], collapse = ", ")
      col_names <- paste0(colnames(data)[empty_cols], collapse = ", ")
      msg <- "Empty values were removed:"
      if (sum(empty_rows) != 0) msg <- paste0(msg, "\n* Rows: ", row_names)
      if (sum(empty_cols) != 0) msg <- paste0(msg, "\n* Columns: ", col_names)
      warning(msg, call. = FALSE)
    }

    ndim <- min(dim(data_clean) - 1)
    dim_names <- paste0("CA", seq_len(ndim))
    row_names <- rownames(data_clean)
    col_names <- colnames(data_clean)

    results <- ca::ca(obj = data_clean)
    sv <- results$sv # Singular values
    eig <- sv^2 # Eigenvalues
    pvar <- eig / sum(eig) * 100 # Percentage of variance
    cvar <- cumsum(pvar) # Cumulative percentage of variance
    tmp <- matrix(
      data = c(eig, pvar, cvar), ncol = 3,
      dimnames = list(dim_names, c("eigenvalue", "pc_variance", "cum_variance"))
    )

    row_marge <- rowSums(data_clean / sum(data_clean))
    row_svd <- results$rowcoord
    row_coords <- row_svd %*% diag(sv)
    row_contrib <- t(t(row_coords^2 * row_marge) / eig) * 100
    z <- list(row_names, dim_names)
    dimnames(row_svd) <- dimnames(row_coords) <- dimnames(row_contrib) <- z

    col_marge <- colSums(data_clean / sum(data_clean))
    col_svd <- results$colcoord
    col_coords <- col_svd %*% diag(sv)
    col_contrib <- t(t(col_coords^2 * col_marge) / eig) * 100
    z <- list(col_names, dim_names)
    dimnames(col_svd) <- dimnames(col_coords) <- dimnames(col_contrib) <- z

    j <- seq_len(min(dimensions, ndim, na.rm = TRUE))
    .CA(
      id = arkhe::get_id(object),
      data = data_clean,
      row_names = row_names,
      row_coordinates = row_coords[, j],
      row_contribution = row_contrib[, j],
      row_distances = results$rowdist,
      row_inertia = results$rowinertia,
      row_svd = row_svd[, j],
      column_names = col_names,
      column_coordinates = col_coords[, j],
      column_contribution = col_contrib[, j],
      column_distances = results$colinertia,
      column_inertia = results$colinertia,
      column_svd = col_svd[, j],
      singular_values = sv,
      eigenvalues = round(tmp, 3)
    )
  }
)

#' @export
#' @rdname ca
#' @aliases predict_ca,CA,CountMatrix-method
setMethod(
  f = "predict_ca",
  signature = signature(object = "CA", data = "CountMatrix"),
  definition = function(object, data, ...) {
    # data <- as.matrix(data)
    data <- data[, object[["column_names"]]]

    ## Get CA standard coordinates (SVD)
    svd <- object[["column_svd"]]
    data <- data / rowSums(data)

    crossprod(t(data), svd)
  }
)
