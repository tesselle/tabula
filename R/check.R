# HELPERS TO CHECK DATA INPUT

checkIfInf <- function(x) {
  n <- sum(is.infinite(x))
  if(n == 1) {
    "an infinite value was detected"
  } else if(n > 1) {
    sprintf("%d infinite values were detected", n)
  } else {
    NULL
  }
}
checkIfNA <- function(x) {
  n <- sum(is.na(x))
  if(n == 1) {
    "a missing value was detected"
  } else if(n > 1) {
    sprintf("%d missing values were detected", n)
  } else {
    NULL
  }
}
checkIfNaN <- function(x) {
  n <- sum(is.nan(x))
  if(n == 1) {
    "a non-number value was detected"
  } else if(n > 1) {
    sprintf("%d non-number values were detected", n)
  } else {
    NULL
  }
}

checkDim <- function(x, expected) {
  n <- dim(x)
  if(!identical(n, expected)) {
    sprintf("should be %d", paste(expected, collapse = " x "))
  } else {
    NULL
  }
}
checkLength <- function(x, expected) {
  n <- length(x)
  if(n != expected) {
    sprintf("should be of length %d, not %d", expected, n)
  } else {
    NULL
  }
}
checkLengths <- function(x, expected) {
  n <- lengths(x)
  m <- paste(n, collapse = ", ")
  if(missing(expected)) {
    if(!isEqual(n)) {
      sprintf("elements should have the same length (%s)", m)
    } else {
      NULL
    }
  } else if(!identical(n, expected)) {
    expected <- if(length(expected) == 1) rep(expected, n) else expected
    sprintf("elements should be of length %d (not %s)", expected, m)
  } else {
    NULL
  }
}
checkNames <- function(x, expected) {
  if(length(x) != 0) {
    n <- names(x)
    if(missing(expected)) {
      if(length(n) == 0) {
        "should be named"
      } else {
        NULL
      }
    } else {
      m <- paste(sQuote(expected), collapse = ", ")
      if(!identical(n, expected)) {
        sprintf("should have the following names: %s", m)
      } else {
        NULL
      }
    }
  } else {
    NULL
  }
}
checkColumns <- function(x, j, names) {
  n <- ncol(x)
  if(n != j) {
    sprintf("should have %d columns, not %d", j, n)
  } else if(!identical(colnames(x), names)) {
    sprintf("should have the following column names: %s",
            paste(sQuote(names), collapse = ", "))
  } else {
    NULL
  }
}
checkRows <- function(x, i, names) {
  n <- nrow(x)
  if(n != i) {
    sprintf("should have %d rows, not %d", i, n)
  } else if(!identical(rownames(x), names)) {
    sprintf("should have the following row names: %s",
            paste(sQuote(names), collapse = ", "))
  } else {
    NULL
  }
}
checkClass <- function(x, expected = c("character", "factor", "integer",
                                       "logical", "numeric")) {
  expected <- match.arg(expected, several.ok = FALSE)
  # Ignore this test if length(x) == 0
  if(length(x) != 0 & class(x) != expected) {
    sprintf("should be of %s class", expected)
  } else {
    NULL
  }
}
checkType <- function(x, expected = c("character", "integer",
                                      "logical", "numeric")) {
  expected <- match.arg(expected, several.ok = FALSE)
  # Ignore this test if length(x) == 0
  if(length(x) != 0 & mode(x) != expected) {
    sprintf("should be of %s type", expected)
  } else {
    NULL
  }
}

checkIfPositive <- function(x, strict = FALSE, na.rm = TRUE) {
  if (!isPositive(x, strict = strict, na.rm = na.rm)) {
    "positive values are expected"
  } else {
    NULL
  }
}
checkIfWholeNumber <- function(x) {
  if (sum(!isWholeNumber(x)) != 0) {
    "whole numbers are expected"
  } else {
    NULL
  }
}
checkIfConstantSum <- function(x) {
  if (!isEqual(rowSums(x, na.rm = TRUE))) {
    "should have constant row sums"
  } else {
    NULL
  }
}

checkIfBinaryMatrix <- function(x) {
  # Check only if matrix dimensions > 1 x 1
  if (!identical(dim(x), as.integer(c(1, 1)))) {
    if (isBinary(x)) {
      "You should consider using an incidence matrix."
    } else {
      NULL
    }
  } else {
   NULL
  }
}
checkIfSquare <- function(x) {
  if (!isSquare(x)) {
    "should be a square matrix"
  } else {
    NULL
  }
}
checkIfSymmetric <- function(x) {
  if (!isSymmetric(x)) {
    "should be a symmetric matrix"
  } else if(!identical(rownames(x), colnames(x))) {
    "should have the same row and column names"
  } else {
    NULL
  }
}
checkIfUUID <- function(x) {
  if(!is.character(x))
    stop("x should be a character vector.")

  if(!anyNA(x)) {
    n <- nchar(x)
    if(any(n != 36)) {
      sprintf("should be 36 characters long string, not %d", n)
    } else {
      NULL
    }
  } else {
    NULL
  }
}

#' Error
#'
#' Raises errors.
#' @param object An object to which error messages are related.
#' @param errors A \code{\link{character}} vector giving the error messages.
#' @return
#'  Raises an error if \code{errors} is of non-zero length, returns \code{TRUE}
#'  if not.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
returnSlotErrors <- function(object, errors) {
  if(!is.list(errors))
    stop("A list of error messages is expected.")

  errors <- errors[which(lengths(errors) > 0)]
  if (length(errors) != 0) {
    messages <- lapply(X = names(errors), FUN = function(slot, errors) {
      paste(sQuote(slot), errors[[slot]])
    }, errors = errors)

    stop(
      sprintf(
        "%s\n  %s",
        dQuote(class(object)),
        paste(unlist(messages), sep = " ", collapse = "\n  ")
      ),
      call. = FALSE
    )
  } else {
    return(TRUE)
  }
}
