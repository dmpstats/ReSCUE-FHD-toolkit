#' Plot heightshift results as an interactive line chart
#'
#' @description Renders a plotly line chart from the output of [heightshift()],
#'   with one line per FHD. A dotted reference line and annotation mark the
#'   current turbine position (shift = 0).
#'
#' @param heightshift_data Named list with elements `prob` and `perc`, as
#'   returned by [heightshift()]. Each element is a character matrix with an
#'   `fhd_id` column and one column per height shift (e.g. `"-40m"`, `"+5m"`).
#' @param metric Character. Controls which matrix is plotted and how the y-axis
#'   is labelled. One of:
#'   \describe{
#'     \item{`"prob"`}{Raw proportion of birds at collision-risk height (CRH).}
#'     \item{`"perc_change"`}{Percentage change in CRH proportion relative to
#'       the current turbine position (shift = 0).}
#'   }
#'
#' @return A plotly figure object.
#' @noRd
plot_heightshift <- function(
  heightshift_data,
  metric = c("prop", "perc_change"),
  airgap,
  rotor_radius
) {
  metric <- match.arg(metric)
  type <- if (metric == "perc_change") "perc" else "prob"
  mat <- heightshift_data[[type]]

  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  df_long <- tidyr::pivot_longer(
    df,
    cols = -fhd_id,
    names_to = "shift",
    values_to = "value"
  )
  df_long$value <- as.numeric(df_long$value)
  # parse "+40m" / "-40m" -> numeric
  df_long$shift_m <- as.numeric(gsub("m", "", df_long$shift))

  ylab <- if (metric == "perc_change") {
    "Change in average<br>proportion at CRH (%)"
  } else {
    "Average proportion at CRH"
  }

  fhd_ids <- unique(df_long$fhd_id)
  n_fhds <- length(fhd_ids)
  palette <- RColorBrewer::brewer.pal(max(3L, n_fhds), "Set1")[seq_len(n_fhds)]

  plt <- plotly::plot_ly()

  for (i in seq_along(fhd_ids)) {
    fid <- fhd_ids[i]
    sub <- df_long[df_long$fhd_id == fid, ]
    line_col <- palette[i]

    plt <- plt |>
      plotly::add_lines(
        data = sub,
        x = ~shift_m,
        y = ~value,
        name = fid,
        line = list(color = line_col, width = 2),
        showlegend = TRUE
      ) |>
      plotly::add_markers(
        data = sub,
        x = ~shift_m,
        y = ~value,
        marker = list(
          color = paste0(line_col, "80"),
          size = 6,
          line = list(color = line_col, width = 1)
        ),
        showlegend = FALSE,
        hoverinfo = "text",
        text = ~ paste0(
          "<b>",
          fhd_id,
          "</b><br>",
          "Shift: ",
          shift_m,
          " m<br>",
          "Value: ",
          value
        )
      )
  }

  plt |>
    plotly::layout(
      xaxis = list(title = "Air Gap Shift (m)"),
      yaxis = list(title = ylab),
      legend = list(title = list(text = "FHD ID")),
      # shapes = list(
      #   list(
      #     type = "line",
      #     x0 = 0,
      #     x1 = 0,
      #     y0 = 0,
      #     y1 = 1,
      #     yref = "paper",
      #     line = list(color = "grey", dash = "dot", width = 1)
      #   )
      # ),
      annotations = list(
        # list(
        #   x = 0,
        #   y = 1,
        #   xref = "x",
        #   yref = "paper",
        #   text = paste0("current specification"),
        #   showarrow = FALSE,
        #   textangle = -90,
        #   xanchor = "left",
        #   yanchor = "top",
        #   font = list(color = "grey", size = 10)
        # ),
        list(
          x = 0.95,
          y = 1,
          xref = "paper",
          yref = "paper",
          xanchor = "right",
          yanchor = "bottom",
          text = paste0(
            "<b>Air gap</b>: ",
            airgap,
            " m | <b>Rotor radius</b>: ",
            rotor_radius,
            " m"
          ),
          showarrow = FALSE,
          font = list(color = "grey", size = 11)
        )
      )
    )
}
