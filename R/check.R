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
#' @name check
#' @keywords internal error
#' @noRd

check_type <- function(x, expected) {
  arg <- deparse(substitute(x))
  predicate <- switch(
    expected,
    list = is_list,
    atomic = is_atomic,
    vector = is_vector,
    numeric = is_numeric,
    integer = is_integer,
    double = is_double,
    character = is_character,
    logical = is_logical,
    stop("Can't find a predicate for this type: ", expected, call. = FALSE)
  )
  if (!predicate(x)) {
    msg <- sprintf("%s must be %s; not %s.", sQuote(arg), expected, typeof(x))
    throw_error("error_bad_type", msg)
  }
}

check_scalar <- function(x, expected) {
  arg <- deparse(substitute(x))
  predicate <- switch(
    expected,
    list = is_scalar_list,
    atomic = is_scalar_atomic,
    vector = is_scalar_vector,
    numeric = is_scalar_numeric,
    integer = is_scalar_integer,
    double = is_scalar_double,
    character = is_scalar_character,
    logical = is_scalar_logical,
    stop("Can't find a predicate for this scalar: ", expected, call. = FALSE)
  )
  if (!predicate(x)) {
    msg <- sprintf("%s must be a scalar (%s).", sQuote(arg), expected)
    throw_error("error_bad_scalar", msg)
  }
}

check_length <- function(x, expected) {
  arg <- deparse(substitute(x))
  n <- length(x)
  if (n != expected) {
    msg <- sprintf("%s must be of length %d; not %s.", sQuote(arg), expected, n)
    throw_error("error_bad_dimension", msg)
  }
}

check_lengths <- function(x, expected = NULL) {
  arg <- deparse(substitute(x))
  n <- lengths(x)
  m <- paste0(n, collapse = ", ")
  if (is.null(expected)) {
    if (!is_equal(n)) {
      msg <- sprintf("Elements of %s must have the same length; not %s.",
                     sQuote(arg), m)
      throw_error("error_bad_dimension", msg)
    }
  } else {
    expected <- as.integer(expected)
    if (is_empty(n) || !identical(n, expected)) {
      msg <- sprintf("Elements of %s must have the following lengths %s; not %s.",
                     sQuote(arg), paste0(expected, collapse = ", "), m)
      throw_error("error_bad_dimension", msg)
    }
  }
}

check_dimension <- function(x, expected) {
  arg <- deparse(substitute(x))
  n <- dim(x)
  expected <- as.integer(expected)
  if (!identical(n, expected)) {
    msg <- sprintf("%s must be of dimension %s; not %s.", sQuote(arg),
                   paste0(expected, collapse = " x "),
                   paste0(n, collapse = " x "))
    throw_error("error_bad_dimension", msg)
  }
}

check_names <- function(x, expected = NULL, margin = c(1, 2)) {
  arg <- deparse(substitute(x))
  if (is.array(x) || is.data.frame(x)) {
    n <- dimnames(x)[[margin]]
    if (is_scalar_numeric(margin)) {
      mar <- ifelse(margin == 1, "row ", "column ")
    } else {
      mar <- "dim"
    }
  } else {
    n <- names(x)
    mar <- ""
  }
  if (is.null(expected)) {
    if (is_empty(n)) {
      msg <- sprintf("%s must have %snames.", sQuote(arg), mar)
      throw_error("error_bad_names", msg)
    }
  } else if (is_empty(n) || !identical(n, expected)) {
    msg <- sprintf("%s must have the following %snames: %s.",
                   sQuote(arg), mar, paste0(expected, collapse = ", "))
    throw_error("error_bad_names", msg)
  }
}

check_missing <- function(x) {
  arg <- deparse(substitute(x))
  n <- sum(is.na(x))
  if (n > 0) {
    msg <- sprintf("%s must not contain missing values (%d detected).",
                   sQuote(arg), n)
    throw_error("error_data_missing", msg)
  }
}

check_infinite <- function(x) {
  arg <- deparse(substitute(x))
  n <- sum(is.infinite(x))
  if (n > 0) {
    msg <- sprintf("%s must not contain infinite values (%d detected).",
                   sQuote(arg), n)
    throw_error("error_data_infinite", msg)
  }
}

check_numbers <- function(x, expected = c("positive", "whole", "odd"), ...) {
  arg <- deparse(substitute(x))
  predicate <- switch(
    expected,
    positive = is_positive,
    whole = is_whole,
    odd = is_odd,
    stop("Can't find a predicate for this: ", expected, call. = FALSE)
  )
  if (!all(predicate(x, ...))) {
    msg <- sprintf("%s must contain %s numbers.", sQuote(arg), expected)
    throw_error("error_bad_number", msg)
  }
}

check_constant <- function(x) {
  arg <- deparse(substitute(x))
  # Check rowSums for array
  if (is.matrix(x) || is.data.frame(x)) {
    x <- rowSums(x)
    if (!is_equal(x)) {
      msg <- sprintf("%s must have constant row sums.", sQuote(arg))
      throw_error("error_bad_value", msg)
    }
  } else {
    if (!is_equal(x)) {
      msg <- sprintf("%s must be constant.", sQuote(arg))
      throw_error("error_bad_value", msg)
    }
  }
}

check_matrix <- function(x, expected = c("square", "symmetric")) {
  arg <- deparse(substitute(x))
  predicate <- switch(
    expected,
    square = is_square,
    symmetric = is_symmetric,
    stop("Can't find a predicate for this matrix: ", expected, call. = FALSE)
  )
  if (!predicate(x)) {
    msg <- sprintf("%s must be a %s matrix.", sQuote(arg), expected)
    throw_error("error_bad_matrix", msg)
  }
}

check_uuid <- function(x) {
  arg <- deparse(substitute(x))
  if (!is_uuid(x)) {
    msg <- sprintf("%s must be an UUID.", sQuote(arg))
    throw_error("error_bad_uuid", msg)
  }
}

# =================================================================== conditions
#' Conditions
#'
#' @param message A \code{\link{character}} string specifying the error
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
#' @keywords internal error
#' @noRd

throw_error <- function(.subclass, message, call = NULL, ...) {
  # TODO: gettext
  err <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c(.subclass, "error", "condition")
  )
  stop(err)
}

catch_conditions <- function(expr) {
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
  return(conditions)
}

throw_error_class <- function(object, errors) {
  errors <- compact(is_empty, errors)
  if (!is_empty(errors)) {
    messages <- lapply(
      X = names(errors),
      FUN = function(slot, errors) {
        vapply(X = errors[[slot]], FUN = `[[`, FUN.VALUE = "character", 1)
      },
      errors = errors
    )
    error_msg <- sprintf("%s object initialization:\n*  %s",
                         dQuote(class(object)),
                         paste0(unlist(messages), collapse = "\n*  "))
    err <- structure(
      list(message = error_msg, call = NULL),
      class = c("error_class_initialize", "error", "condition")
    )
    stop(err)
  } else {
    TRUE
  }
}

throw_message_class <- function(class, verbose = getOption("verbose")) {
  msg <- structure(
    list(
      message = sprintf("%s instance initialization...\n", dQuote(class))
    ),
    class = c("message_class_initialize", "message", "condition")
  )
  if (verbose) message(msg)
}
