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
    cat(
      sprintf("<DateModel: %s>", object@id),
      "Modelled event date:",
      sprintf("- Residual standard error: %f", round(sum_up$sigma, digits = 2)),
      sprintf("- Multiple R-squared: %f", round(sum_up$r.squared, 5)),
      sprintf("- Adjusted R-squared: %f", round(sum_up$adj.r.squared, 5)),
      sep = "\n"
    )
  }
)

# DiversityIndex ===============================================================
setMethod(
  f = "show",
  signature = "DiversityIndex",
  definition = function(object) {
    cat(
      sprintf("<%s: %s>", class(object), object@id),
      sprintf("- Method: %s", object@method),
      sep = "\n"
    )
    print(methods::as(object, "data.frame"))
  }
)

# PermutationOrder =============================================================
setMethod(
  f = "show",
  signature = "PermutationOrder",
  definition = function(object) {
    k <- 50
    rows <- strtrim(paste0(object@rows, collapse = " "), k)
    columns <- strtrim(paste0(object@columns, collapse = " "), k)
    cat(
      sprintf("<PermutationOrder: %s>", object@id),
      "Permutation order for matrix seriation:",
      sprintf("- Row order: %s", paste0(rows, "...")),
      sprintf("- Column order: %s", paste0(columns, "...")),
      sprintf("- Method: %s", object@method),
      sep = "\n"
    )
  }
)
