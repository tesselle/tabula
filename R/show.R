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
    cat("Partial bootstrap CA seriation refinement:", "\n",
        "  Cutoff values: ", cut, "\n",
        "  Rows to keep: ", keep[1], " of ", total[1], " (", pc[1], "%)", "\n",
        "  Columns to keep: ", keep[2], " of ", total[2], " (", pc[2], "%)",
        sep = "")
  }
)

# DateModel ====================================================================
setMethod(
  f = "show",
  signature = "DateModel",
  definition = function(object) {
    cat("Modelled event date:\n",
        "  R2:", stats::summary.lm(object@model)$r.squared, "\n",
        "  Residual standard deviation:", round(object@residual, digits = 0), "years\n",
        "  CI:", object@level * 100, "%\n",
        sep = " ")
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
    cat(paste(m, "x", p, "presence/absence data matrix:", sep = " "), "\n",
        sep = " ")
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
    cat(paste(m, "x", p, "co-occurrence matrix:", sep = " "), "\n",
        sep = " ")
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
    cat(paste(m, "x", p, "count data matrix:", sep = " "), "\n", sep = " ")
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
    cat(paste(m, "x", p, "frequency data matrix:", sep = " "), "\n", sep = " ")
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
    cat(paste(m, "x", p, "(dis)similarity matrix:", sep = " "), "\n",
        "  Method:", object@method, "\n", sep = " ")
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
      paste(paste(object@rows[1:k], collapse = " "),
            "... (", m-k, " more)", sep ="")
    } else {
      object@rows
    }
    columns <- if (p > k) {
      paste(paste(object@columns[1:k], collapse = " "),
            "... (", p-k, " more)", sep ="")
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
