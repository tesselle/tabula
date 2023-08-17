# CLASSES VALIDATION
#' @include AllClasses.R
NULL

# DiversityIndex ===============================================================
setValidity(
  Class = "DiversityIndex",
  method = function(object) {
    ## Get data
    .Data <- object@.Data
    labels <- object@labels
    size <- object@size
    data <- object@data
    method <- object@method

    n <- nrow(data)

    ## Validate
    cnd <- list(
      # arkhe::validate(arkhe::assert_length(.Data, n)),
      # arkhe::validate(arkhe::assert_length(labels, n)),
      arkhe::validate(arkhe::assert_length(size, n)),
      arkhe::validate(arkhe::assert_scalar(method, "character"))
    )

    ## Return conditions, if any
    arkhe::check_class(object, cnd)
  }
)

# RarefactionIndex =============================================================
setValidity(
  Class = "RarefactionIndex",
  method = function(object) {
    ## Get data
    .Data <- object@.Data
    labels <- object@labels
    size <- object@size
    method <- object@method

    m <- nrow(.Data)
    n <- ncol(.Data)

    ## Validate
    cnd <- list(
      arkhe::validate(arkhe::assert_length(labels, m)),
      arkhe::validate(arkhe::assert_length(size, n)),
      arkhe::validate(arkhe::assert_scalar(method, "character"))
    )

    ## Return conditions, if any
    arkhe::check_class(object, cnd)
  }
)
