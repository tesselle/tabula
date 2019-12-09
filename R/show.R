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

# DiversityIndex ===============================================================
setMethod(
  f = "show",
  signature = "DiversityIndex",
  definition = function(object) {
    cat(sprintf("<%s: %s>\n", class(object), object@method))
    print(methods::as(object, "data.frame"))
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
