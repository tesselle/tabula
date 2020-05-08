# CLASSES VALIDATION
#' @include AllClasses.R
NULL
#
# # =============================================================== DiversityIndex
# setValidity(
#   Class = "DiversityIndex",
#   method = function(object) {
#     # Get data
#     id <- object@id
#     index <- object@index
#     size <- object@size
#     jackknife <- object@jackknife
#     boostrap <- object@boostrap
#     simulation <- object@simulation
#     method <- object@method
#     n <- length(index)
#
#     errors <- list(
#       # Check id
#       # arkhe::catch_conditions(arkhe:::check_uuid(id)),
#       # Check index
#       arkhe::catch_conditions(arkhe::check_missing(index)),
#       arkhe::catch_conditions(arkhe::check_infinite(index)),
#       # Check size
#       arkhe::catch_conditions(arkhe::check_missing(size)),
#       arkhe::catch_conditions(arkhe::check_infinite(size)),
#       arkhe::catch_conditions(arkhe::check_length(size, n)),
#       # Check method
#       arkhe::catch_conditions(arkhe::check_scalar(method, "character")),
#       arkhe::catch_conditions(arkhe::check_missing(method))
#     )
#     # Check jackknife
#     if (nrow(jackknife) > 0 && n > 0) {
#       errors <- append(
#         errors,
#         list(
#           arkhe::catch_conditions(arkhe::check_missing(jackknife)),
#           arkhe::catch_conditions(arkhe::check_dimension(jackknife, c(n, 3)))
#         )
#       )
#     }
#     # Check boostrap
#     if (nrow(boostrap) > 0 && n > 0) {
#       errors <- append(
#         errors,
#         list(
#           arkhe::catch_conditions(arkhe::check_missing(boostrap)),
#           arkhe::catch_conditions(arkhe::check_dimension(boostrap, c(n, 5)))
#         )
#       )
#     }
#     # Check simulation
#     if (nrow(simulation) > 0 && n > 0) {
#       errors <- append(
#         errors,
#         list(
#           arkhe::catch_conditions(arkhe::check_missing(simulation))
#           # arkhe::catch_conditions(arkhe::check_dimension(simulation, 4))
#         )
#       )
#     }
#
#     # Return errors if any
#     arkhe::check_class(object, errors)
#   }
# )
#
# # ======================================================================= BootCA
# setValidity(
#   Class = "BootCA",
#   method = function(object) {
#     # Get data
#     id <- object@id
#     rows <- object@rows
#     columns <- object@columns
#     lengths <- object@lengths
#     cutoff <- object@cutoff
#     keep <- object@keep
#
#     errors <- list(
#       # Check id
#       # arkhe::catch_conditions(arkhe::check_uuid(id)),
#       # Check rows
#       arkhe::catch_conditions(arkhe::check_length(rows, 3)),
#       arkhe::catch_conditions(arkhe::check_constant(lengths(rows))),
#       arkhe::catch_conditions(arkhe::check_names(rows, c("id", "x", "y"))),
#       arkhe::catch_conditions(arkhe::check_type(rows[[1]], "character")),
#       arkhe::catch_conditions(arkhe::check_type(rows[[2]], "numeric")),
#       arkhe::catch_conditions(arkhe::check_type(rows[[3]], "numeric")),
#       unlist(lapply(
#         X = rows,
#         FUN = function(x) {
#           c(arkhe::catch_conditions(arkhe::check_missing(x)),
#             arkhe::catch_conditions(arkhe::check_infinite(x)))
#         }
#       )),
#       # Check columns
#       arkhe::catch_conditions(arkhe::check_length(columns, 3)),
#       arkhe::catch_conditions(arkhe::check_constant(lengths(columns))),
#       arkhe::catch_conditions(arkhe::check_names(columns, c("id", "x", "y"))),
#       arkhe::catch_conditions(arkhe::check_type(columns[[1]], "character")),
#       arkhe::catch_conditions(arkhe::check_type(columns[[2]], "numeric")),
#       arkhe::catch_conditions(arkhe::check_type(columns[[3]], "numeric")),
#       unlist(lapply(
#         X = columns,
#         FUN = function(x) {
#           c(arkhe::catch_conditions(arkhe::check_missing(x)),
#             arkhe::catch_conditions(arkhe::check_infinite(x)))
#         }
#       )),
#       # Check lenghts
#       arkhe::catch_conditions(arkhe::check_length(lengths, expected = 2)),
#       arkhe::catch_conditions(arkhe::check_type(lengths[[1]], "numeric")),
#       arkhe::catch_conditions(arkhe::check_type(lengths[[2]], "numeric")),
#       unlist(lapply(
#         X = lengths,
#         FUN = function(x) {
#           c(arkhe::catch_conditions(arkhe::check_missing(x)),
#             arkhe::catch_conditions(arkhe::check_infinite(x)))
#         }
#       )),
#       # Check cutoff
#       arkhe::catch_conditions(arkhe::check_length(cutoff, 2)),
#       arkhe::catch_conditions(arkhe::check_missing(cutoff)),
#       arkhe::catch_conditions(arkhe::check_infinite(cutoff)),
#       # Check keep
#       unlist(lapply(
#         X = keep,
#         FUN = function(x) {
#           c(arkhe::catch_conditions(arkhe::check_type(x, "integer")),
#             arkhe::catch_conditions(arkhe::check_missing(x)))
#         }
#       )),
#       arkhe::catch_conditions(arkhe::check_length(keep, 2))
#     )
#
#     # Return errors if any
#     arkhe::check_class(object, errors)
#   }
# )
#
# # ==================================================================== DateModel
# setValidity(
#   Class = "DateModel",
#   method = function(object) {
#     errors <- c()
#     # Get data
#     id <- object@id
#     counts <- object@counts
#     level <- object@level
#     # model <- object@model
#     rows <- object@rows
#     columns <- object@columns
#     accumulation <- object@accumulation
#
#     errors <- list(
#       # Check id
#       # arkhe::catch_conditions(arkhe::check_uuid(id)),
#       # Check counts
#       arkhe::catch_conditions(arkhe::check_type(counts, "numeric")),
#       arkhe::catch_conditions(arkhe::check_missing(counts)),
#       arkhe::catch_conditions(arkhe::check_infinite(counts)),
#       # Check level
#       arkhe::catch_conditions(arkhe::check_scalar(level, "numeric")),
#       arkhe::catch_conditions(arkhe::check_missing(level)),
#       arkhe::catch_conditions(arkhe::check_infinite(level)),
#       # Check rows
#       arkhe::catch_conditions(arkhe::check_type(rows, "numeric")),
#       arkhe::catch_conditions(arkhe::check_missing(rows)),
#       arkhe::catch_conditions(arkhe::check_infinite(rows)),
#       arkhe::catch_conditions(
#         arkhe::check_dimnames(rows, list(NULL, c("date", "lower", "upper", "error")))
#       ),
#       # Check columns
#       arkhe::catch_conditions(arkhe::check_type(columns, "numeric")),
#       arkhe::catch_conditions(arkhe::check_missing(columns)),
#       arkhe::catch_conditions(arkhe::check_infinite(columns)),
#       arkhe::catch_conditions(
#         arkhe::check_dimnames(columns, list(NULL, c("date", "lower", "upper", "error")))
#       ),
#       # Check accumulation
#       arkhe::catch_conditions(arkhe::check_type(accumulation, "numeric")),
#       arkhe::catch_conditions(arkhe::check_missing(accumulation)),
#       arkhe::catch_conditions(arkhe::check_infinite(accumulation)),
#       arkhe::catch_conditions(
#         arkhe::check_dimnames(accumulation, list(NULL, c("date", "error")))
#       )
#     )
#
#     # Return errors if any
#     arkhe::check_class(object, errors)
#   }
# )
#
# # ============================================================= PermutationOrder
# setValidity(
#   Class = "PermutationOrder",
#   method = function(object) {
#     # Get data
#     id <- object@id
#     rows <- object@rows
#     columns <- object@columns
#     method <- object@method
#
#     errors <- list(
#       # Check id
#       # arkhe::catch_conditions(arkhe::check_uuid(id)),
#       # Check rows
#       arkhe::catch_conditions(arkhe::check_missing(rows)),
#       arkhe::catch_conditions(arkhe::check_numbers(rows, "positive",
#                                                    strict = TRUE)),
#       # Check columns
#       arkhe::catch_conditions(arkhe::check_missing(columns)),
#       arkhe::catch_conditions(arkhe::check_numbers(columns, "positive",
#                                                    strict = TRUE)),
#       # Check method
#       arkhe::catch_conditions(arkhe::check_scalar(method, "character")),
#       arkhe::catch_conditions(arkhe::check_missing(method))
#     )
#
#     # Return errors if any
#     arkhe::check_class(object, errors)
#   }
# )
