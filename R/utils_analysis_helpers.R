#' make_fhd_summary
#'
#' @description A utils function for generating a summary-table of the FHDs
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
make_fhd_summary <- function(
  fhd_data,
  id_col = "fhd_id",
  height_col = "height",
  prob_col = "probability",
  draw_col = "draw_id",
  risk_min = 50,
  risk_max = 100
) {
  if (is.null(fhd_data) || nrow(fhd_data) == 0) {
    cli::cli_abort(
      "fhd_data is NULL or empty. Please provide a valid data frame."
    )
  }
  if (!all(c(id_col, height_col, prob_col, draw_col) %in% colnames(fhd_data))) {
    cli::cli_abort(
      "One or more specified columns do not exist in fhd_data. Please check the column names."
    )
  }

  # Create a new .df with only the relevant columns
  fhd_data <- data.frame(
    fhd_id = fhd_data[[id_col]],
    height = fhd_data[[height_col]],
    prob = fhd_data[[prob_col]],
    draw_id = fhd_data[[draw_col]]
  )

  # Determine which rows are in the risk-zone
  fhd_data <- fhd_data |>
    dplyr::mutate(
      in_risk_zone = dplyr::if_else(
        height >= risk_min & height <= risk_max,
        TRUE,
        FALSE
      )
    )

  # Generate estimates for how many are in the risk-zone, including quantiles. Calculate for each draw, and then summarise
  fhd_summary <- fhd_data |>
    dplyr::group_by(fhd_id, draw_id) |>
    dplyr::summarise(
      total_prob = sum(prob, na.rm = TRUE),
      riskzone_prob = sum(prob[in_risk_zone], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(fhd_id) |>
    dplyr::summarise(
      q50_risk = quantile(riskzone_prob, probs = 0.50, na.rm = TRUE) |>
        round(3),
      q25_risk = quantile(riskzone_prob, probs = 0.25, na.rm = TRUE) |>
        round(3),
      q75_risk = quantile(riskzone_prob, probs = 0.75, na.rm = TRUE) |>
        round(3)
    ) |>
    dplyr::rename(
      "FHD ID" = fhd_id,
      "Median" = q50_risk,
      "25 %tile" = q25_risk,
      "75 %tile" = q75_risk
    )

  return(fhd_summary)
}
