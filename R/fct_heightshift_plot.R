#' Plot heightshift results as an interactive line chart
#'
#' @description Renders a plotly line chart from the output of [heightshift()],
#'   with one line per FHD. When `show_as_percentages = TRUE`, markers are
#'   coloured green (risk decrease) or red (risk increase) to mirror the table
#'   colour-coding. A dotted reference line and annotation mark the current
#'   turbine position (shift = 0).
#'
#' @param heightshift_data Named list with elements `prob` and `perc`, as
#'   returned by [heightshift()]. Each element is a character matrix with an
#'   `fhd_id` column and one column per height shift (e.g. `"-40m"`, `"+5m"`).
#' @param show_as_percentages Logical. If `TRUE`, uses the `perc` matrix and
#'   labels the y-axis as percentage change; if `FALSE`, uses `prob`.
#'
#' @return A plotly figure object.
#' @noRd
heightshift_plot <- function(heightshift_data, show_as_percentages = TRUE) {
  type <- if (show_as_percentages) "perc" else "prob"
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

  ylab <- if (show_as_percentages) {
    "Change in proportion at CRH (%)"
  } else {
    "Proportion at CRH"
  }

  fhd_ids <- unique(df_long$fhd_id)
  n_fhds <- length(fhd_ids)
  palette <- RColorBrewer::brewer.pal(max(3L, n_fhds), "Set1")[seq_len(n_fhds)]

  plt <- plotly::plot_ly()

  for (i in seq_along(fhd_ids)) {
    fid <- fhd_ids[i]
    sub <- df_long[df_long$fhd_id == fid, ]
    line_col <- palette[i]

    # Mirror table colour-coding: green = risk down, red = risk up (% mode only)
    marker_cols <- if (show_as_percentages) {
      ifelse(
        sub$value < 0, "#2ca02c",
        ifelse(sub$value > 0, "#d62728", "#888888")
      )
    } else {
      line_col
    }

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
          color = marker_cols,
          size = 6,
          line = list(color = line_col, width = 1)
        ),
        showlegend = FALSE,
        hoverinfo = "text",
        text = ~paste0(
          "<b>", fhd_id, "</b><br>",
          "Shift: ", shift_m, " m<br>",
          "Value: ", value
        )
      )
  }

  plt |>
    plotly::layout(
      xaxis = list(title = "Height shift (m)"),
      yaxis = list(title = ylab),
      legend = list(title = list(text = "FHD")),
      shapes = list(
        list(
          type = "line",
          x0 = 0, x1 = 0,
          y0 = 0, y1 = 1,
          yref = "paper",
          line = list(color = "grey", dash = "dot", width = 1)
        )
      ),
      annotations = list(
        list(
          x = 0,
          y = 1,
          xref = "x",
          yref = "paper",
          text = "current position",
          showarrow = FALSE,
          textangle = -90,
          xanchor = "left",
          yanchor = "top",
          font = list(color = "grey", size = 10)
        )
      )
    )
}
