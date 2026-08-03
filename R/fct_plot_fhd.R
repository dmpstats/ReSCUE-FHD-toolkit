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
    plotdat$group_col <- as.character(plotdat[[plot_by_cov[1]]])
    if (length(plot_by_cov) > 1) {
      for (cov in plot_by_cov[-1]) {
        plotdat$group_col <- paste(
          plotdat$group_col,
          plotdat[[cov]],
          sep = "."
        )
      }
    }
    if (!is.null(index)) {
      plotdat$group_col <- paste0(index, ".", plotdat$group_col)
    }
  }

  plotdat_summed <- plotdat |>
    dplyr::group_by(group_col, height) |>
    dplyr::summarise(
      f_id = dplyr::first(f_id),
      lc = quantile(prob, 0.025, na.rm = TRUE),
      prob = mean(prob, na.rm = TRUE),
      uc = quantile(prob, 0.975, na.rm = TRUE),
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
