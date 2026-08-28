add_fhd <- function(
  plot,
  fhd_data,
  id_col = "unique_fhd",
  height_col = "height",
  draw_col = "draw_id",
  prob_col = "probability",
  plot_by_cov = NULL,
  index = NULL,
  compact_legend = TRUE
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
    # compact legends if requested, otherwise use the covariate values as-is for the legend labels
    if (compact_legend) {
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
    } else {
      plotdat$group_col <- plotdat[[id_col]]
      # # add linebreak to covars
      # plotdat$group_col <- sub(" ", "\n", plotdat$group_col)
    }
  }

  plotdat_summed <- plotdat |>
    dplyr::group_by(group_col, height) |>
    dplyr::summarise(
      f_id = dplyr::first(f_id),
      lc = quantile(prob, 0.025, na.rm = TRUE),
      uc = quantile(prob, 0.975, na.rm = TRUE),
      prob = mean(prob, na.rm = TRUE),
      unique_colour = dplyr::first(colours),
      .groups = "drop"
    )

  # Create a color mapping from group_col to hex codes
  color_map <- structure(
    plotdat_summed$unique_colour,
    names = plotdat_summed$group_col
  )

  plot <- plot |>
    plotly::add_ribbons(
      data = plotdat_summed,
      x = ~height,
      ymin = ~lc,
      ymax = ~uc,
      fillcolor = ~unique_colour,
      line = list(color = ~unique_colour, opacity = 0, width = 0),
      # Share a legendgroup with the matching line trace (below) so that,
      # combined with `legend$groupclick = "togglegroup"` in fhd_baseplot(),
      # toggling the line's legend entry also hides/shows its ribbon —
      # without adding a second legend entry for the ribbon itself.
      legendgroup = ~group_col,
      opacity = 0.2,
      hoverinfo = "text",
      text = ~ paste(
        "FHD ID:",
        f_id,
        # "<br>Group:",
        # group_col,
        "<br>Height:",
        height,
        "<br>Probability:",
        round(prob, 4)
      ),
      showlegend = FALSE
    ) |>
    plotly::add_lines(
      data = plotdat_summed,
      x = ~height,
      y = ~prob,
      name = ~group_col,
      legendgroup = ~group_col,
      line = list(
        width = 2,
        color = ~unique_colour
      ),
      hoverinfo = "text",
      text = ~ paste(
        "FHD ID:",
        f_id,
        # "<br>Group:",
        # group_col,
        "<br>Height:",
        height,
        "<br>Probability:",
        round(prob, 4)
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
      #title = "Flight Height Distributions",
      title = NA,
      xaxis = list(title = "Height"),
      yaxis = list(title = "Probability"),
      legend = list(
        title = list(text = "FHD ID"),
        # Draw the legend inside the top-right of the plotting area, rather
        # than in the (wide) margin to the right of the plot.
        x = 0.99,
        y = 0.99,
        xanchor = "right",
        yanchor = "top",
        bgcolor = "rgba(255, 255, 255, 0.7)",
        bordercolor = "rgba(0, 0, 0, 0.2)",
        borderwidth = 1,
        # Clicking a legend entry toggles every trace sharing its
        # legendgroup, so a line's ribbon (same group, showlegend = FALSE)
        # hides/shows together with the line — without a duplicate entry.
        groupclick = "togglegroup"
      )
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
        fill = 'none',
        line = list(color = 'red', dash = 'dot'),
        name = 'Risk Zone'
      )
  }

  p1
}
