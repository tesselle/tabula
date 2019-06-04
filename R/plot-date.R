# PLOT DATES
#' @include AllGenerics.R AllClasses.R
NULL

#' @export
#' @rdname plotDate-method
#' @aliases plotDate,AbundanceMatrix-method
setMethod(
  f = "plotDate",
  signature = signature(object = "AbundanceMatrix"),
  definition = function(object, select = 1, sort = "dsc") {
    # Validation
    checkScalar(sort, expected = "character")
    # Selection
    cases <- rownames(object)
    index <- if (is.character(select)) {
      which(cases %in% select)
    } else {
      as.numeric(select)
    }

    # Get dates
    dates <- object@dates %>%
      as.data.frame() %>%
      dplyr::mutate(
        y = dplyr::row_number(),
        id = factor(rownames(.), levels = unique(rownames(.))),
        min = .data$value - .data$error,
        max = .data$value + .data$error
      ) %>%
      dplyr::filter(stats::complete.cases(.)) %>%
      dplyr::slice(index)
    if (nrow(dates) == 0)
      stop("No dates were found!", call. = FALSE)

    if (!is.null(sort)) {
      sort <- match.arg(sort, choices = c("asc", "dsc"), several.ok = FALSE)
      dates <- switch (
        sort,
        asc = dplyr::arrange(dates, .data$value),
        dsc = dplyr::arrange(dates, dplyr::desc(.data$value))
      )
      dates %<>% dplyr::mutate(y = dplyr::row_number())
    }

    # Set error bar height
    error_height <- ifelse(dates$error == 0, 0, 0.5)

    ggplot(data = dates, mapping = aes_string(color = "id", fill = "id")) +
      geom_errorbarh(mapping = aes_string(xmin = "min", xmax = "max", y = "y"),
                     height = error_height) +
      geom_point(mapping = aes_string(x = "value", y = "y"),
                 shape = 21, size = 2) +
      scale_y_continuous(breaks = dates$y, labels = dates$id) +
      labs(x = "Date", y = "", fill = "Assemblage", color = "Assemblage")
  }
)

#' @export
#' @rdname plotDate-method
#' @aliases plotDate,DateModel-method
setMethod(
  f = "plotDate",
  signature = c(object = "DateModel"),
  definition = function(object, type = c("activity", "tempo"),
                        event = FALSE, select = 1, n = 500) {
    # Validation
    type <- match.arg(type, several.ok = FALSE)
    checkScalar(event, expected = "logical")
    checkScalar(n, expected = "numeric")
    n <- as.integer(n)

    # Selection
    cases <- rownames(object@rows)
    index <- if (is.character(select)) {
      which(cases %in% select)
    } else {
      as.numeric(select)
    }
    k <- length(index)
    if (k == 0)
      stop("Wrong selection.", call. = FALSE)

    # Get data
    rows <- as.data.frame(object@rows)
    row_dates <- rows$date
    row_lower <- rows$lower
    row_upper <- rows$upper
    row_errors <- rows$error
    columns <- as.data.frame(object@columns)
    col_dates <- columns$date
    col_errors <- columns$error
    date_range <- seq(from = min(row_lower), to = max(row_upper),
                      length.out = n)

    # Event date
    plot_event <- NULL
    if (type == "activity" && event) {
      date_event <- mapply(
        FUN = stats::dnorm,
        mean = row_dates[index],
        sd = row_errors[index],
        MoreArgs = list(x = date_range),
        SIMPLIFY = TRUE
      )
      colnames(date_event) <- cases[index]

      row_data <- cbind.data.frame(date = date_range, date_event) %>%
        tidyr::gather(key = "assemblage", value = "density", -date)
      plot_event <- geom_line(data = row_data, color = "black")
    }

    # Accumulation time
    # Weighted sum of the fabric dates
    counts <- object@counts[index, , drop = FALSE]
    freq <- counts / rowSums(counts)
    # Tempo vs activity plot
    fun <- switch(
      type,
      activity = stats::dnorm,
      tempo = stats::pnorm
    )
    col_density <- mapply(
      FUN = fun,
      mean = col_dates,
      sd = col_errors,
      MoreArgs = list(date_range),
      SIMPLIFY = TRUE
    )
    date_acc <- apply(
      X = freq,
      MARGIN = 1,
      FUN = function(x, density) {
        colSums(t(density) * as.numeric(x))
      },
      density = col_density
    )

    col_data <- cbind.data.frame(date = date_range, date_acc) %>%
      tidyr::gather(key = "assemblage", value = "density", -date)

    plot_accumulation <- switch(
      type,
      activity = geom_area(data = col_data, fill = "darkgrey",
                           color = "darkgrey", alpha = 0.7),
      tempo = geom_line(data = col_data, color = "black"),
      NULL
    )

    # Facet if more than one assemblage is selected
    plot_facet <- NULL
    if (k > 1) {
      plot_facet <- facet_wrap(. ~ assemblage, nrow = k, scales = "free_y")
    }

    ggplot(mapping = aes_string(x = "date", y = "density")) +
      plot_accumulation + plot_event + plot_facet +
      labs(x = "Date", y = "Density")
  }
)
