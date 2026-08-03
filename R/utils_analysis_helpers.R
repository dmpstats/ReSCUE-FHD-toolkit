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
      q25_risk = (100 *
        quantile(riskzone_prob, probs = 0.25, na.rm = TRUE)) |>
        round(2),
      q50_risk = (100 *
        quantile(riskzone_prob, probs = 0.50, na.rm = TRUE)) |>
        round(2),
      q75_risk = (100 *
        quantile(riskzone_prob, probs = 0.75, na.rm = TRUE)) |>
        round(2)
    ) |>
    dplyr::rename(
      "Risk perc. (25%)" = q25_risk,
      "Risk perc. (50%)" = q50_risk,
      "Risk perc. (75%)" = q75_risk
    )

  return(fhd_summary)
}
