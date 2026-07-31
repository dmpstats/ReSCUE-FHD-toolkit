#' plot_fhd
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#'
#' @import plotly
#' @noRd
plot_fhd <- function(
  datalist,
  id_col = "fhd_id",
  height_col = "height",
  draw_col = "draw_id",
  prob_col = "probability",
  risk_min = NULL,
  risk_max = NULL,
  cols_by_level = NULL
) {
  plotdat <- data.frame(
    f_id = datalist[[id_col]],
    draw_id = datalist[[draw_col]],
    height = datalist[[height_col]],
    prob = datalist[[prob_col]]
  )

  # For now, we'll plot the means grouped by f_id
  plotdat_summed <- plotdat |>
    dplyr::group_by(f_id, height) |>
    dplyr::summarise(prob = mean(prob, na.rm = TRUE), .groups = "drop")

  # Create the plotly plot
  p1 <- plotly::plot_ly() |>
    plotly::add_lines(
      data = plotdat_summed,
      x = ~height,
      y = ~prob,
      color = ~ as.factor(f_id),
      colors = "Set1",
      line = list(width = 2),
      hoverinfo = "text",
      text = ~ paste(
        "fhd_id:",
        f_id,
        "<br>Height:",
        height,
        "<br>Probability:",
        prob
      )
    ) |>
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
        y = c(0, 0, max(plotdat_summed$prob), max(plotdat_summed$prob), 0),
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

add_fhd <- function(
  plot,
  fhd_data,
  id_col = "fhd_id",
  height_col = "height",
  draw_col = "draw_id",
  prob_col = "probability",
  plot_by_cov = NULL
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
    # Convert plot_by_cov to character vector if it's a single string
    plot_by_cov <- as.character(plot_by_cov)

    # Build the group_col by concatenating covariate names and values
    # Start with the first covariate
    plotdat$group_col <- paste0(plot_by_cov[1], "=", plotdat[[plot_by_cov[1]]])

    # Add remaining covariates if there are any
    if (length(plot_by_cov) > 1) {
      for (cov in plot_by_cov[-1]) {
        plotdat$group_col <- paste0(
          plotdat$group_col,
          ", ",
          cov,
          "=",
          plotdat[[cov]]
        )
      }
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
