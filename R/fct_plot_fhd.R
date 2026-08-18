add_fhd <- function(
  plot,
  fhd_data,
  id_col = "fhd_id",
  height_col = "height",
  draw_col = "draw_id",
  prob_col = "probability",
  plot_by_cov = NULL,
  index = NULL
) {
  plotdat <- fhd_data
  plotdat$f_id <- plotdat[[id_col]]
  plotdat$draw_id <- plotdat[[draw_col]]
  plotdat$prob <- plotdat[[prob_col]]
  plotdat$height <- plotdat[[height_col]]

  # If NULL or character0, group by fhd_id only
  if (is.null(plot_by_cov) | length(plot_by_cov) == 0) {
    plotdat$group_col <- fhd_data[[id_col]]
  } else {
    plot_by_cov <- as.character(plot_by_cov)

    # Build compact legend labels: covariate level values joined with ".".
    # Prefix with the FHD index so entries from different FHDs never collide
    # in the plotly legend (e.g. "1.cold.low", "2.cold.low").
    # NAs are omitted to avoid mismatches between FHDs with different covariates.
    plotdat$group_col <- apply(
      plotdat[, plot_by_cov, drop = FALSE],
      1,
      function(x) paste(na.omit(x), collapse = ".")
    )
    if (!is.null(index)) {
      plotdat$group_col <- paste0(index, ".", plotdat$group_col)
    }
  }

  plotdat_summed <- plotdat |>
    dplyr::group_by(group_col, height) |>
    dplyr::summarise(
      f_id = dplyr::first(f_id),
      lc = quantile(prob, 0.025, na.rm = TRUE),
      uc = quantile(prob, 0.975, na.rm = TRUE),
      prob = mean(prob, na.rm = TRUE),
      .groups = "drop"
    )

  plot <- plot |>
    plotly::add_ribbons(
      data = plotdat_summed,
      x = ~height,
      ymin = ~lc,
      ymax = ~uc,
      color = ~ as.factor(group_col),
      colors = "Set1",
      opacity = 0.2,
      hoverinfo = "text",
      text = ~ paste(
        "fhd_id:",
        f_id,
        "<br>Group:",
        group_col,
        "<br>Height:",
        height,
        "<br>Probability:",
        prob
      ),
      showlegend = FALSE
    ) |>
    plotly::add_lines(
      data = plotdat_summed,
      x = ~height,
      y = ~prob,
      color = ~ as.factor(group_col),
      colors = "Set1",
      line = list(width = 2),
      hoverinfo = "text",
      text = ~ paste(
        "fhd_id:",
        f_id,
        "<br>Group:",
        group_col,
        "<br>Height:",
        height,
        "<br>Probability:",
        prob
      )
    )

  plot
}

#' Build a faceted FHD plot using plotly subplots
#'
#' Each unique value of \code{unique_id_col} gets its own subplot panel.
#'
#' @param plot_data Data frame containing all FHD draws.
#' @param id_col Column used as the FHD identifier for \code{add_fhd}.
#' @param unique_id_col Column whose distinct values define the facets
#'   (typically \code{"unique_fhd"}).
#' @param height_col,draw_col,prob_col Column names for height, draw ID, and
#'   probability respectively.
#' @param risk_min,risk_max Risk-zone bounds passed to \code{fhd_baseplot}.
#' @param show_legend Logical; whether to display the plotly legend.
#'
#' @return A plotly subplot figure.
fhd_facet_plot <- function(
  plot_data,
  id_col = "fhd_id",
  unique_id_col = "unique_fhd",
  height_col = "height",
  draw_col = "draw_id",
  prob_col = "probability",
  risk_min = 50,
  risk_max = 100,
  show_legend = TRUE
) {
  unique_ids <- unique(plot_data[[unique_id_col]])

  plots <- lapply(unique_ids, function(uid) {
    subset <- plot_data[plot_data[[unique_id_col]] == uid, ]
    max_prob <- max(subset[[prob_col]], na.rm = TRUE)
    p <- fhd_baseplot(risk_min = risk_min, risk_max = risk_max)
    p <- add_fhd(
      plot = p,
      fhd_data = subset,
      id_col = id_col,
      height_col = height_col,
      draw_col = draw_col,
      prob_col = prob_col
    )
    # Subplot title via layout annotation (converted by subplot() automatically).
    # Y-axis capped to actual data range to avoid empty space from the risk rectangle.
    p |>
      plotly::layout(
        title = list(text = uid, font = list(size = 10)),
        yaxis = list(range = c(0, max_prob * 1.1)),
        showlegend = show_legend
      )
  })

  n <- length(plots)
  ncols <- min(n, 2L)
  nrows <- ceiling(n / ncols)

  plotly::subplot(
    plots,
    nrows = nrows,
    shareX = FALSE,
    shareY = FALSE,
    titleX = TRUE,
    titleY = TRUE,
    margin = 0.07
  )
}

fhd_baseplot <- function(
  risk_min = 50,
  risk_max = 100
) {
  p1 <- plotly::plot_ly() |>
    plotly::layout(
      title = "Flight Height Distributions",
      xaxis = list(title = "Height"),
      yaxis = list(title = "Probability"),
      legend = list(title = list(text = "fhd_id"))
    )

  if (
    !is.null(risk_min) &&
      !is.null(risk_max) &&
      !is.na(risk_min) &&
      !is.na(risk_max)
  ) {
    p1 <- p1 |>
      plotly::add_trace(
        x = c(risk_min, risk_max, risk_max, risk_min, risk_min),
        y = c(0, 0, 1, 1, 0),
        type = 'scatter',
        mode = 'lines',
        fill = 'toself',
        fillcolor = 'rgba(255, 0, 0, 0.2)',
        line = list(color = 'rgba(255, 0, 0, 0)'),
        name = 'Risk Zone'
      )
  }

  p1
}
