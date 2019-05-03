
checkList <- function(x, mode, names = NULL, length = NULL, lengths = NULL,
                      na = FALSE, nan = FALSE, inf = FALSE) {
  if(length(x) != 0) {
    # Validation
    mode <- match.arg(mode, choices = c("character", "logical", "numeric"),
                      several.ok = FALSE)
    length <- as.integer(length)
    lengths <- as.integer(lengths)
    na <- as.logical(na)
    nan <- as.logical(nan)
    inf <- as.logical(inf)
    if (!is.null(names) & !is.null(length)) {
      z <- length(names)
      if (z != length)
        stop(sprintf("%s should be of length %d, not %d",
                     sQuote("names"), length, z))
    }

    # Get values
    arg <- deparse(substitute(x))
    errors <- vector(mode = "character")

    if(!is.list(x)) {
      errors <- sprintf("%s should be a list", sQuote(arg))
    } else {
      # Check names
      if (!is.null(names)) {
        j <- names(x)
        if(!identical(j, names)) {
          errors <- c(
            errors,
            sprintf("Elements of %s should have the following names, %s",
                    sQuote(arg), paste(sQuote(names), collapse = ", "))
          )
        }
      }
      # Check length
      if(!is.null(length)) {
        n <- length(x)
        if(n > length) {
          errors <- c(
            errors,
            sprintf("%s should be a list of %d, not %d", sQuote(arg), length, n)
          )
        }
      }
      # Check lengths
      if(!is.null(lengths)) {
        m <- lengths(x)
        if(any(m > lengths)) {
          errors <- c(
            errors,
            sprintf("All elements of %s should be of length %d, not %s",
                    sQuote(arg), lengths, paste(m, collapse = " or "))
          )
        }
      }
      # Check type
      if(!all(sapply(X = x, FUN = base::mode) %in% mode)) {
        errors <- c(
          errors,
          sprintf("All elements of %s should be of %s type", sQuote(arg), mode)
        )
      }
      # Check if NA
      if(!na & any(unlist(sapply(X = x, FUN = is.na)))) {
        errors <- c(
          errors,
          sprintf("Missing values are not allowed in %s", sQuote(arg))
        )
      }
      if(mode == "numeric") {
        # Check if NaN
        if(!nan & any(unlist(sapply(X = x, FUN = is.nan)))) {
          errors <- c(
            errors,
            sprintf("NaN values are not allowed in %s", sQuote(arg))
          )
        }
        # Check if Inf
        if(!inf & !all(unlist(sapply(X = x, FUN = is.finite)))) {
          errors <- c(
            errors,
            sprintf("Infinite values are not allowed in %s", sQuote(arg))
          )
        }
      }
    }
    return(errors)
  } else {
    return(NULL)
  }
}

checkVector <- function(x, mode, names = NULL, length = NULL, na = FALSE,
                        required = FALSE) {
  # Validation
  mode <- match.arg(mode, choices = c("character", "logical", "numeric",
                                      "integer"), several.ok = FALSE)

  na <- as.logical(na)
  if (!is.null(names) & !is.null(length)) {
    z <- length(names)
    if (z != length)
      stop(sprintf("%s should be of length %d, not %d",
                   sQuote("names"), length, z))
  }
  # Get values
  argument <- sQuote(deparse(substitute(x)))
  errors <- vector(mode = "character")

  if(length(x) == 0) {
    if(required)
      errors <- c(errors, sprintf("%s is missing", argument))
  } else {
    if(!is.vector(x, mode = mode)) {
      errors <- c(
        errors,
        sprintf("%s should be a % vector", argument, mode)
      )
    } else {
      # Check names
      if (!is.null(names)) {
        j <- names(x)
        if(!identical(j, names)) {
          errors <- c(
            errors,
            sprintf("%s should have the following names, %s",
                    argument, paste(sQuote(names), collapse = ", "))
          )
        }
      }
      # Check length
      if(!is.null(length)) {
        n <- length(x)
        if(n > length) {
          errors <- c(
            errors,
            sprintf("%s should be a vector of length %d, not %d",
                    argument, length, n)
          )
        }
      }
      # Check if NA, NaN or Inf
      if(!na) {
        if(mode == "numeric" & !all(is.finite(x))) {
          errors <- c(
            errors,
            sprintf("finite values are expected in %s", argument)
          )
        } else if(anyNA(x)) {
          errors <- c(
            errors,
            sprintf("Missing values are not allowed in %s", argument)
          )
        }
      }
    }

  }
  if(length(errors != 0)) return(errors) else return(NULL)
}

printErrors <- function(object, errors, call = TRUE) {
  stop(
    sprintf(
      "%s\n  %s.",
      dQuote(class(object)),
      paste(errors, sep = " ", collapse = ";\n  ")
    ),
    call. = call
  )
}
