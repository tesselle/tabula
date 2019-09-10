# SHOW METHODS
#' @include AllClasses.R
NULL

# BootCA =======================================================================
setMethod(
  f = "show",
  signature = "BootCA",
  definition = function(object) {
    cut <- paste(round(object@cutoff, digits = 2), c("(rows)", "(columns)"),
                 collapse = " - ", sep = " ")
    keep <- lengths(object@keep)
    total <- lengths(object@lengths)
    pc <- round(keep * 100 / total)
    cat("Partial bootstrap CA seriation refinement:",
        "\n- Cutoff values: ", cut,
        "\n- Rows to keep: ", keep[1], " of ", total[1], " (", pc[1], "%)",
        "\n- Columns to keep: ", keep[2], " of ", total[2], " (", pc[2], "%)",
        sep = "")
  }
)

# DateModel ====================================================================
setMethod(
  f = "show",
  signature = "DateModel",
  definition = function(object) {
    fit <- object@model
    sum_up <- stats::summary.lm(fit)
    cat("Modelled event date:",
        "\n- Residual standard error: ", round(sum_up$sigma, digits = 2),
        "\n- Multiple R-squared: ", round(sum_up$r.squared, 5),
        "\n- Adjusted R-squared: ", round(sum_up$adj.r.squared, 5),
        sep = "")
  }
)

# Logical matrix ===============================================================
setMethod(
  f = "show",
  signature = "IncidenceMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    cat(sprintf("%d x %d presence/absence data matrix:\n(%s)\n",
                m, p, object@id))
    print(data)
  }
)
setMethod(
  f = "show",
  signature = "OccurrenceMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    cat(sprintf("%d x %d co-occurrence matrix:\n(%s)\n", m, p, object@id))
    print(data)
  }
)

# Numeric matrix ===============================================================
setMethod(
  f = "show",
  signature = "CountMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    cat(sprintf("%d x %d count data matrix:\n(%s)\n", m, p, object@id))
    print(data)
  }
)
setMethod(
  f = "show",
  signature = "FrequencyMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    cat(sprintf("%d x %d frequency data matrix:\n(%s)\n", m, p, object@id))
    print(data)
  }
)
setMethod(
  f = "show",
  signature = "SimilarityMatrix",
  definition = function(object) {
    data <- methods::S3Part(object, strictS3 = TRUE, "matrix")
    m <- nrow(data)
    p <- ncol(data)
    cat(sprintf("%d x %d (dis)similarity matrix (%s):\n(%s)\n",
                m, p, object@method, object@id))
    print(data)
  }
)

# PermutationOrder =============================================================
setMethod(
  f = "show",
  signature = "PermutationOrder",
  definition = function(object) {
    m <- length(object@rows)
    p <- length(object@columns)
    k <- 20
    rows <- if (m > k) {
      paste0(paste0(object@rows[seq_len(k)], collapse = " "),
             "... (", m-k, " more)")
    } else {
      object@rows
    }
    columns <- if (p > k) {
      paste0(paste0(object@columns[seq_len(k)], collapse = " "),
             "... (", p-k, " more)")
    } else {
      object@columns
    }
    cat("Permutation order for matrix seriation:", "\n",
        "  Matrix ID:", object@id, "\n",
        "  Row order:", rows, "\n",
        "  Column order:", columns, "\n",
        "  Method:", object@method,
        sep = " "
    )
  }
)
