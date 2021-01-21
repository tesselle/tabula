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
    cat(
      "<BootCA>",
      "Partial bootstrap CA seriation refinement:",
      sprintf("- Cutoff values: %s", cut),
      sprintf("- Rows to keep: %d of %d (%g%%)", keep[1], total[1], pc[1]),
      sprintf("- Columns to keep: %d of %d (%g%%)", keep[2], total[2], pc[2]),
      sep = "\n")
  }
)

# DateModel ====================================================================
setMethod(
  f = "show",
  signature = "DateModel",
  definition = function(object) {
    fit <- object@model
    if (class(fit) == "S4") {
      cat("<DateModel>")
    } else {
      sum_up <- stats::summary.lm(fit)
      cat(
        "<DateModel>",
        "Modelled event date:",
        sprintf("- Residual standard error: %f", round(sum_up$sigma, 2)),
        sprintf("- Multiple R-squared: %f", round(sum_up$r.squared, 5)),
        sprintf("- Adjusted R-squared: %f", round(sum_up$adj.r.squared, 5)),
        sep = "\n"
      )
    }
  }
)

# DiversityIndex ===============================================================
setMethod(
  f = "show",
  signature = "DiversityIndex",
  definition = function(object) {
    cat(sprintf("<%s: %s>\n", class(object), object[["method"]]))
    print(as.data.frame(object))
  }
)

# IncrementTest ===============================================================
setMethod(
  f = "show",
  signature = "IncrementTest",
  definition = function(object) {
    cat(sprintf("<%s: %s>\n", class(object)))
    print(as.data.frame(object))
  }
)

# PermutationOrder =============================================================
setMethod(
  f = "show",
  signature = "PermutationOrder",
  definition = function(object) {
    k <- 50
    rows <- strtrim(paste0(object[["rows"]], collapse = " "), k)
    columns <- strtrim(paste0(object[["columns"]], collapse = " "), k)
    cat(
      sprintf("<%s: %s>", class(object), object[["method"]]),
      "Permutation order for matrix seriation:",
      sprintf("- Row order: %s", paste0(rows, "...")),
      sprintf("- Column order: %s", paste0(columns, "...")),
      sep = "\n"
    )
  }
)
