# CHECK DATA INPUT
#' @include predicates.R
NULL

#' Check data inputs
#'
#' @param x An object to be checked.
#' @param expected An appropriate expected value.
#' @return
#'  Throw an error if any.
#' @author N. Frerebeau
#' @examples
#' \dontrun{
#' object <- 1:3
#' checkType(object, "character")
#' checkScalar(object, "numeric")
#' checkLength(object, 2)
#' checkNames(object)
#' checkNames(object, c("a", "b", "c"))
#'
#' object <- c(1:3, NA, Inf)
#' checkMissing(object)
#' checkInfinite(object)
#'
#' object <- list(1:3, NULL, LETTERS)
#' checkLength(object, 2)
#' checkLengths(object)
#' checkLengths(object, c(3, 1, 26))
#'
#' object <- matrix(-0.1, nrow = 3, ncol = 2)
#' checkDim(object, c(2, 3))
#' checkMatrix(object, "square")
#' checkMatrix(object, "symmetric")
#' checkNumbers(object, "positive", strict = TRUE)
#' checkNumbers(object, "whole")
#' }
#' @name check
#' @keywords internal
NULL

#' @rdname check
checkType <- function(x, expected) {
  arg <- deparse(substitute(x))
  predicate <- switch(
    expected,
    list = isList,
    atomic = isAtomic,
    vector = isVector,
    numeric = isNumeric,
    integer = isInteger,
    double = isDouble,
    character = isCharacter,
    logical = isLogical,
    stop("Can't find a predicate for this type: ", expected, call. = FALSE)
  )
  if (!predicate(x))
    throwError(arg, must = sprintf("be %s", expected), not = typeof(x))
}
#' @rdname check
checkScalar <- function(x, expected) {
  arg <- deparse(substitute(x))
  predicate <- switch(
    expected,
    list = isScalarList,
    atomic = isScalarAtomic,
    vector = isScalarVector,
    numeric = isScalarNumeric,
    integer = isScalarInteger,
    double = isScalarDouble,
    character = isScalarCharacter,
    logical = isScalarLogical,
    stop("Can't find a predicate for this scalar: ", expected, call. = FALSE)
  )
  if (!predicate(x))
    throwError(arg, must = sprintf("be a scalar (%s).", expected))
}
#' @rdname check
checkLength <- function(x, expected) {
  arg <- deparse(substitute(x))
  n <- length(x)
  if (n != expected)
    throwError(arg, must = sprintf("be of length %d", expected), not = n)
}
#' @rdname check
checkLengths <- function(x, expected) {
  arg <- deparse(substitute(x))
  n <- lengths(x)
  m <- paste0(n, collapse = ", ")
  if (missing(expected)) {
    if (!isEqual(n)) {
      throwError(arg, must = "have equal lengths", not = m)
    }
  } else if (isEmpty(n) || any(n != expected)) {
    ex <- paste0(expected, collapse = ", ")
    throwError(arg, must = sprintf("have the following lengths: %s", ex),
                    not = m)
  }
}
#' @rdname check
checkDim <- function(x, expected) {
  arg <- deparse(substitute(x))
  n <- dim(x)
  if (any(n != expected)) {
    m <-  paste0(expected, collapse = " x ")
    throwError(arg, must = sprintf("be of dimension %s", m),
                    not = paste0(n, collapse = " x "))
  }
}
#' @rdname check
checkNames <- function(x, expected) {
  arg <- deparse(substitute(x))
  n <- names(x)
  if (missing(expected)) {
    if (isEmpty(n)) {
      throwError(arg, must = "be named.")
    }
  } else if (isEmpty(n) || any(n != expected)) {
    throwError(
      arg, must = sprintf("have the following names: %s.",
                          paste0(sQuote(expected), collapse = ", "))
    )
  }
}
#' @rdname check
checkColnames <- function(x, expected) {
  arg <- deparse(substitute(x))
  n <- colnames(x)
  if (missing(expected)) {
    if (isEmpty(n)) {
      throwError(arg, must = "have column names.")
    }
  } else if (isEmpty(n) || any(n != expected)) {
    throwError(
      arg, must = sprintf("have the following column names: %s.",
                          paste0(sQuote(expected), collapse = ", "))
    )
  }
}
#' @rdname check
checkMissing <- function(x) {
  arg <- deparse(substitute(x))
  n <- sum(is.na(x))
  if (n > 0)
    throwError(
      arg, must = sprintf("not contain missing values (%d detected).", n)
    )
}
#' @rdname check
checkInfinite <- function(x) {
  arg <- deparse(substitute(x))
  n <- sum(is.infinite(x))
  if (n > 0)
    throwError(
      arg, must = sprintf("not contain infinite values (%d detected).", n)
    )
}
#' @rdname check
checkNumbers <- function(x, expected, ...) {
  arg <- deparse(substitute(x))
  predicate <- switch(
    expected,
    positive = isPositive,
    whole = isWholeNumber,
    stop("Can't find a predicate for this: ", expected, call. = FALSE)
  )
  if (!all(predicate(x, ...)))
    throwError(arg, must = sprintf("contain %s numbers.", expected))
}
#' @rdname check
checkConstant <- function(x, expected, na.rm = TRUE) {
  arg <- deparse(substitute(x))
  # Check rowSums for array
  if (is.matrix(x) || is.data.frame(x)) {
    x <- rowSums(x)
    if (!isEqual(x))
      throwError(arg, must = "have constant row sums")
  } else {
    if (!isEqual(x))
      throwError(arg, must = "be constant")
  }
}
#' @rdname check
checkMatrix <- function(x, expected) {
  arg <- deparse(substitute(x))
  predicate <- switch(
    expected,
    square = isSquare,
    symmetric = isSymmetric,
    stop("Can't find a predicate for this matrix: ", expected, call. = FALSE)
  )
  if (!predicate(x))
    throwError(arg, must = sprintf("be a %s matrix.", expected))
}

#' Conditions
#'
#' @param expr An expression.
#' @param arg A \code{\link{character}} string specifying the argument name.
#' @param must,not A \code{\link{character}} string specifying the error
#'  message.
#' @param call The call.
#' @param object An object to which error messages are related.
#' @param errors A \code{\link{character}} vector giving the error messages.
#' @param ... Extra arguments.
#' @return
#'  Throw an error if \code{errors} is of non-zero length, returns \code{TRUE}
#'  if not.
#' @author N. Frerebeau
#' @name conditions
#' @keywords internal
NULL

#' @rdname conditions
catchConditions <- function(expr) {
  conditions <- list()
  add_mess <- function(cnd) {
    conditions <<- append(conditions, list(cnd))
    suppressMessages(cnd)
  }
  add_warn <- function(cnd) {
    conditions <<- append(conditions, list(cnd))
    suppressWarnings(cnd)
  }
  add_err <- function(cnd) {
    conditions <<- append(conditions, list(cnd))
  }

  tryCatch(
    error = add_err,
    withCallingHandlers(
      message = add_mess,
      warning = add_warn,
      expr
    )
  )
  conditions
}
#' @rdname conditions
throwError <- function(arg, must, not = NULL, call = NULL, ...) {
  msg <- sprintf("`%s` must %s", arg, must)
  if (!is.null(not)) {
    msg <- sprintf("%s; not %s.", msg, as.character(not))
  }

  err <- structure(
    list(
      message = msg,
      call = call,
      ...
    ),
    class = c("error", "condition")
  )
  stop(err)
}
#' @rdname conditions
formatErrors <- function(object, errors) {
  errors <- compact(isEmpty, errors)
  if (!isEmpty(errors)) {
    messages <- lapply(
      X = names(errors),
      FUN = function(slot, errors) {
        vapply(X = errors[[slot]], FUN = `[[`, FUN.VALUE = "character", 1)
      },
      errors = errors
    )

    stop(
      sprintf(
        "%s object initialization:\n*  %s",
        dQuote(class(object)),
        paste0(unlist(messages), collapse = "\n*  ")
      ),
      call. = FALSE
    )
  } else {
    TRUE
  }
}
