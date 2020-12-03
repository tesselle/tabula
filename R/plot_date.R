# PLOT DATES
#' @include AllClasses.R AllGenerics.R
NULL

#' @export
#' @rdname plot_date
#' @aliases plot_date,DataMatrix-method
setMethod(
  f = "plot_date",
  signature = signature(object = "DataMatrix"),
  definition = function(object, select = NULL, sort = "dsc") {
    # Get dates
    dates <- rownames_to_column(arkhe::get_dates(object),
                                factor = TRUE, id = "id")

    dates <- cbind.data.frame(
      dates,
      y = seq_len(nrow(dates)),
      min = dates$value - dates$error,
      max = dates$value + dates$error
    )
    dates <- dates[stats::complete.cases(dates), ]

    # Selection
    cases <- dates[["id"]]
    index <- if (is.null(select)) {
      seq_along(cases)
    } else if (is.character(select)) {
      which(cases %in% select)
    } else {
      as.numeric(select)
    }

    if (length(index) == 0) stop("Wrong selection.", call. = FALSE)
    dates <- dates[index, ]

    if (!is.null(sort)) {
      sort <- match.arg(sort, choices = c("asc", "dsc"), several.ok = FALSE)
      dates <- switch (
        sort,
        asc = dates[order(dates$value), ],
        dsc = dates[order(dates$value, decreasing = TRUE), ]
      )
      dates[["y"]] <- seq_len(nrow(dates))
    }

    # Set error bar height
    error_height <- ifelse(dates$error == 0, 0, 0.5)

    aes_plot <- ggplot2::aes(color = .data$id, fill = .data$id)
    aes_point <- ggplot2::aes(x = .data$value, y = .data$y)
    aes_err <- ggplot2::aes(xmin = .data$min, xmax = .data$max, y = .data$y)
    ggplot2::ggplot(data = dates, mapping = aes_plot) +
      ggplot2::geom_errorbarh(mapping = aes_err, height = error_height) +
      ggplot2::geom_point(mapping = aes_point, shape = 21, size = 2) +
      ggplot2::scale_y_continuous(breaks = dates$y, labels = dates$id) +
      ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
      ggplot2::labs(x = "Date", y = "",
                    fill = "Assemblage", color = "Assemblage")
  }
)

#' @export
#' @rdname plot_date
#' @aliases plot_date,DateModel-method
setMethod(
  f = "plot_date",
  signature = c(object = "DateEvent"),
  definition = function(object, type = c("activity", "tempo"),
                        event = FALSE, select = 1, n = 500) {
    # Validation
    type <- match.arg(type, several.ok = FALSE)
    n <- as.integer(n)

    # Get data
    rows <- as.data.frame(object[["row_events"]])
    row_dates <- rows$date
    row_lower <- rows$lower
    row_upper <- rows$upper
    row_errors <- rows$error
    columns <- as.data.frame(object[["column_events"]])
    col_dates <- columns$date
    col_errors <- columns$error
    date_range <- seq(from = min(row_lower), to = max(row_upper),
                      length.out = n)
    # Selection
    cases <- rownames(rows)
    index <- if (is.null(select)) {
      seq_along(cases)
    } else if (is.character(select)) {
      which(cases %in% select)
    } else {
      as.numeric(select)
    }
    k <- length(index)
    if (k == 0)
      stop("Wrong selection.", call. = FALSE)

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

      # Build a long table for ggplot2
      row_stacked <- wide2long(date_event)
      row_data <- cbind.data.frame(date = date_range, row_stacked)
      colnames(row_data) <- c("date", "density", "assemblage", "type")
      plot_event <- ggplot2::geom_line(data = row_data, color = "black")
    }

    # Accumulation time
    # Weighted sum of the fabric dates
    counts <- object[["data"]][index, , drop = FALSE]
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

    # Build a long table for ggplot2
    col_stacked <- wide2long(date_acc)
    col_data <- cbind.data.frame(date = date_range, col_stacked)
    colnames(col_data) <- c("date", "density", "assemblage", "type")

    # ggplot
    plot_accumulation <- switch(
      type,
      activity = ggplot2::geom_area(fill = "darkgrey", color = "darkgrey",
                                    alpha = 0.7),
      tempo = ggplot2::geom_line(color = "black"),
      NULL
    )

    # Facet if more than one assemblage is selected
    plot_facet <- NULL
    if (k > 1) {
      plot_facet <- ggplot2::facet_wrap(ggplot2::vars(.data$assemblage),
                                        nrow = k, scales = "free_y")
    }

    aes_plot <- ggplot2::aes(x = .data$date, y = .data$density)
    ggplot2::ggplot(data = col_data, mapping = aes_plot) +
      plot_accumulation + plot_event + plot_facet +
      ggplot2::labs(x = "Date", y = "Density")
  }
)
