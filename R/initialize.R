# CLASSES INITIALIZATION
#' @include AllClasses.R
NULL
#
# # ==================================================================== DateModel
# setMethod(
#   f = "initialize",
#   signature = "DateModel",
#   definition = function(.Object, ..., id, counts, level, model,
#                         rows, columns, accumulation) {
#
#     mtx <- matrix(0, 0, 4, dimnames = list(NULL, c("date", "lower", "upper", "error")))
#     acc <- matrix(0, 0, 2, dimnames = list(NULL, c("date", "error")))
#
#     .Object@id <- if (missing(id)) arkhe:::generate_uuid() else id
#     .Object@counts <- if (missing(counts)) matrix(0, 0, 0) else counts
#     .Object@level <- if (missing(level)) numeric(1) else level
#     .Object@model <- if (missing(model)) stats::lm(0 ~ 0) else model
#     .Object@rows <- if (missing(rows)) mtx else rows
#     .Object@columns <- if (missing(columns)) mtx else columns
#     .Object@accumulation <- if (missing(accumulation)) acc else accumulation
#
#     .Object <- methods::callNextMethod()
#     methods::validObject(.Object)
#     .Object
#   }
# )
#
# # =============================================================== DiversityIndex
# setMethod(
#   f = "initialize",
#   signature = "DiversityIndex",
#   definition = function(.Object, ..., id, index, size, jackknife, boostrap,
#                         simulation, method) {
#
#     jack <- matrix(0, 0, 3, dimnames = list(NULL, c("mean", "bias", "error")))
#     boot <- matrix(0, 0, 5, dimnames = list(NULL, c("min","Q05", "mean", "Q95", "max")))
#     sim <- matrix(0, 0, 4, dimnames = list(NULL, c("size", "mean", "lower", "upper")))
#
#     .Object@id <- if (missing(id)) arkhe:::generate_uuid() else id
#     .Object@index <- if (missing(index)) numeric(0) else index
#     .Object@size <- if (missing(size)) integer(0) else as.integer(size)
#     .Object@jackknife <- if (missing(jackknife)) jack else jackknife
#     .Object@boostrap <- if (missing(boostrap)) boot else boostrap
#     .Object@simulation <- if (missing(simulation)) sim else simulation
#     .Object@method <- if (missing(method)) "unknown" else method
#
#     .Object <- methods::callNextMethod()
#     methods::validObject(.Object)
#     .Object
#   }
# )
#
# # ======================================================================= BootCA
# setMethod(
#   f = "initialize",
#   signature = "BootCA",
#   definition = function(.Object, ..., id, rows, columns, lengths,
#                         cutoff, keep) {
#
#     .Object@id <- if (missing(id)) arkhe:::generate_uuid() else id
#     if (missing(rows)) {
#       rows <- list(id = character(0), x = numeric(0), y = numeric(0))
#     } else {
#       rows
#     }
#     .Object@rows <- rows
#     if (missing(columns)) {
#       columns <- list(id = character(0), x = numeric(0), y = numeric(0))
#     } else {
#       columns
#     }
#     .Object@columns <- columns
#     .Object@lengths <- if (missing(lengths)) {
#       list(numeric(0), numeric(0))
#     } else {
#       mapply(FUN = `names<-`,
#              lengths, list(unique(rows$id), unique(columns$id)),
#              SIMPLIFY = FALSE)
#     }
#     .Object@cutoff <- if (missing(cutoff)) c(0, 0) else cutoff
#     .Object@keep <- if (missing(keep)) list(integer(0), integer(0)) else keep
#
#     .Object <- methods::callNextMethod()
#     methods::validObject(.Object)
#     .Object
#   }
# )
#
# # ============================================================= PermutationOrder
# setMethod(
#   f = "initialize",
#   signature = "PermutationOrder",
#   definition = function(.Object, ..., id, rows, columns, method) {
#
#     .Object@id <- if (missing(id)) arkhe:::generate_uuid() else id
#     .Object@rows <- if (missing(rows)) integer(0) else rows
#     .Object@columns <- if (missing(columns)) integer(0) else columns
#     .Object@method <- if (missing(method)) "unknown" else method
#
#     .Object <- methods::callNextMethod()
#     methods::validObject(.Object)
#     .Object
#   }
# )
