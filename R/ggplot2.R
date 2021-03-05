# GGPLOT2 UTILITIES
NULL

# Scales =======================================================================
scale_x_matrix <- function(x, name = ggplot2::waiver(), position = "top") {
  ggplot2::scale_x_continuous(
    name = name,
    expand = c(0, 0),
    limits = c(0.5, ncol(x) + 0.5),
    breaks = seq_len(ncol(x)),
    labels = colnames(x),
    position = position
  )
}
scale_y_matrix <- function(x, name = ggplot2::waiver(), position = "left") {
  ggplot2::scale_y_continuous(
    name = name,
    trans = "reverse",
    expand = c(0, 0),
    limits = c(nrow(x) + 0.5, 0.5),
    breaks = seq_len(nrow(x)),
    labels = rownames(x),
    position = position
  )
}

# Themes =======================================================================
theme_tabula <- function() {
  ggplot2::theme_gray() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x.top = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_text(
        angle = 45,
        hjust = 0,
        vjust = 0
      ),
      legend.key = ggplot2::element_rect(fill = "white"),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid = ggplot2::element_blank()
    )
}
