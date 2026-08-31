#' heightshift
#'
#' @description Evaluate how FHD risk probabilities change across a range of turbine
#' rotor heights. This function simulates shifting the turbine rotor vertically
#' through the air column and quantifies the resulting changes in collision probability
#' for each FHD, both as absolute probabilities and as percentage changes relative
#' to the baseline turbine position.
#'
#' @details
#' The function operates by:
#' \enumerate{
#'   \item Creating a series of hypothetical rotor positions within the data range,
#'   each maintaining the same rotor diameter as the baseline configuration.
#'   \item For each height shift, computing the probability of FHD risk by aggregating
#'   collision data within the shifted rotor window.
#'   \item Averaging collision probabilities across multiple draws (if present
#'   in the data) to account for uncertainty in FHD encounter positions.
#'   \item Calculating percentage changes relative to the baseline turbine height.
#' }
#'
#' @param fhd_data A data frame containing FHD collision probability data, typically
#' output from an FHD analysis workflow. Expected structure: one row per (FHD, height,
#' draw) combination with associated probability values.
#' @param height_col Character string specifying the column name containing turbine
#' rotor heights (m). Default is `"height"`.
#' @param prob_col Character string specifying the column name containing probability
#' or cumulative probability values for FHD collisions. Default is `"prob"`.
#' @param id_col Character string specifying the column name containing unique FHD
#' identifiers. Default is `"fhd_id"`.
#' @param draw_id_col Character string specifying the column name containing Monte Carlo
#' draw identifiers. Used to stratify calculations across uncertainty samples.
#' Default is `"draw_id"`.
#' @param risk_min Numeric scalar specifying the minimum height of the baseline turbine
#' rotor, AKA the airgap (m). Defines the lower boundary of the rotor disk.
#' @param risk_max Numeric scalar specifying the maximum height of the baseline turbine
#' rotor (m). Defines the upper boundary of the rotor disk. The rotor diameter
#' is calculated as `risk_max - risk_min`.
#' @param round A 2-element integer vector `c(prob_places, perc_places)` specifying
#' decimal places for rounding the output. The first element rounds probability values,
#' the second rounds percentage changes. If `NULL`, no rounding is applied.
#' Default is `c(4, 2)`.
#' @param restrict_bounds Logical. If `TRUE` (default), restricts height shifts to
#' positions within ±40 m of the baseline `risk_min`, preventing unrealistic turbine
#' placements. If `FALSE`, all shifts within the data range are considered.
#' @param condensed_table Logical. If `FALSE` (default), height shifts are applied
#' in 1-meter increments. If `TRUE`, shifts use 5-meter increments, producing a
#' more compact output table.
#'
#' @return A list with two elements:
#' \describe{
#' \item{\code{prob}}{A matrix of absolute FHD collision probabilities. Rows represent
#'   FHD identifiers; columns represent height shifts (labeled with meter offsets from
#'   baseline, e.g., `"+0m"`, `"+5m"`, `"-10m"`). The matrix includes
#'   an `fhd_id` column prepended at index 1. An attribute `"true_fhd_col"`
#'   marks the column index of the baseline turbine position.}
#'   \item{\code{perc}}{A matrix of percentage changes in FHD collision probability
#'   relative to the baseline configuration. Same structure as `prob`, with values
#'   representing percent change (e.g., `+5` means 5% increase). Enables direct
#'   comparison of FHD risk sensitivity across the rotor height range.}
#' }
#'
heightshift <- function(
  fhd_data,
  height_col = "height",
  prob_col = "prob",
  id_col = "fhd_id",
  draw_id_col = "draw_id",
  risk_min = 50,
  risk_max = 100,
  round = c(4, 2),
  restrict_bounds = TRUE,
  condensed_table = FALSE
) {
  # Create a new .df with only the relevant columns
  fhd_data <- data.frame(
    fhd_id = fhd_data[[id_col]],
    height = fhd_data[[height_col]],
    prob = fhd_data[[prob_col]],
    draw_id = fhd_data[[draw_id_col]]
  )

  max_height <- max(fhd_data$height, na.rm = TRUE)
  rotor_diameter <- risk_max - risk_min
  step <- if (condensed_table) 5 else 1

  # lowest positive height given step and airgap
  lowest_height <- risk_min - (floor(risk_min / step) * step)

  heightshifts <- tibble::tibble(
    shifted_risk_min = seq(
      lowest_height,
      max_height - rotor_diameter,
      by = step
    ),
    shifted_risk_max = seq(
      lowest_height + rotor_diameter,
      max_height,
      by = step
    )
  )

  # Restrict to shifts within ±40 m of the true turbine position
  if (restrict_bounds) {
    heightshifts <- heightshifts[
      abs(heightshifts$shifted_risk_min - risk_min) <= 40,
    ]
    rownames(heightshifts) <- NULL
  }

  # Positional index of the true turbine setting in the (possibly restricted) table
  true_fhd_id <- which(heightshifts$shifted_risk_min == risk_min)

  # Pre-split data by fhd_id so we only subset the data frame once per FHD
  data_split <- split(fhd_data, fhd_data$fhd_id)
  fhd_ids <- names(data_split)

  # Vectorised computation: for each FHD build a draw × height matrix, then
  # for every height-shift window sum the relevant columns and average over draws.
  hs_risk_min <- heightshifts$shifted_risk_min
  hs_risk_max <- heightshifts$shifted_risk_max
  n_shifts <- nrow(heightshifts)

  fhd_prob_matrix <- vapply(
    fhd_ids,
    function(fid) {
      sub <- data_split[[fid]]
      # Pivot to a draw × height matrix (values = summed probability per cell)
      wide <- tapply(sub$prob, list(sub$draw_id, sub$height), sum, default = 0)
      heights_in_mat <- as.numeric(colnames(wide))

      # For each height-shift window, pick columns in range, sum per draw, then average
      vapply(
        seq_len(n_shifts),
        function(j) {
          in_range <- heights_in_mat >= hs_risk_min[j] &
            heights_in_mat <= hs_risk_max[j]
          if (!any(in_range)) {
            return(NA_real_)
          }
          mean(
            rowSums(wide[, in_range, drop = FALSE], na.rm = TRUE),
            na.rm = TRUE
          )
        },
        numeric(1)
      )
    },
    numeric(n_shifts)
  )

  # vapply returns a n_shifts × n_fhds matrix; transpose to n_fhds × n_shifts
  fhd_prob_matrix <- t(fhd_prob_matrix)

  # Label columns with the actual metre shift from the true turbine position
  actual_shifts <- heightshifts$shifted_risk_min - risk_min
  col_labels <- paste0(ifelse(actual_shifts >= 0, "+", ""), actual_shifts, "m")
  dimnames(fhd_prob_matrix) <- list(fhd_ids, col_labels)

  # Percentage change relative to the true turbine position
  fhd_perc_matrix <- sweep(
    fhd_prob_matrix,
    1,
    fhd_prob_matrix[, true_fhd_id],
    FUN = function(x, y) (x - y) / y * 100
  )

  # Round everything, if requested
  if (!is.null(round)) {
    fhd_prob_matrix <- round(fhd_prob_matrix, round[1])
    fhd_perc_matrix <- round(fhd_perc_matrix, round[2])
  }

  # Add the FHD IDs as the first column
  fhd_perc_matrix <- cbind(
    fhd_id = rownames(fhd_perc_matrix),
    fhd_perc_matrix
  )
  fhd_prob_matrix <- cbind(
    fhd_id = rownames(fhd_prob_matrix),
    fhd_prob_matrix
  )

  # Store the column index of the true turbine setting (offset by 1 for fhd_id col)
  attr(fhd_prob_matrix, "true_fhd_col") <- true_fhd_id + 1L
  attr(fhd_perc_matrix, "true_fhd_col") <- true_fhd_id + 1L

  return(
    list(
      prob = fhd_prob_matrix,
      perc = fhd_perc_matrix
    )
  )
}
