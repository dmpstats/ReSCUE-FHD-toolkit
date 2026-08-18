#' heightshift
#'
#' @description A fct function for evaluating changing FHD risk-zones as the
#'  height of the turbine is increased or decreased.
#'
#' @param fhd_data A data frame containing the FHD risk-zone data for a given turbine height.
#' @param height_col The column name for the turbine height values.
#' @param prob_col The column name for the probability values.
#' @param id_col The column name for the unique FHD identifiers.
#' @param draw_id_col The column name for the draw identifiers.
#' @param risk_min The minimum height of the turbine rotor (in meters).
#' @param risk_max The maximum height of the turbine rotor (in meters).
#' @param round A 2-length vector of integers specifying the number of decimal places to round the probability and percentage values to, respectively. If NULL, no rounding is performed.
#'
#'
#' @return A matrix whereby each row represents a unique FHD identifier and each
#'  column represents a unique turbine height. The values in the
#' matrix represent the probability of FHD risk for each
#' identifier at each height.
#'
#' @noRd
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
  turbine_height <- risk_max - risk_min
  step <- if (condensed_table) 5 else 1

  heightshifts <- data.frame(
    risk_min = seq(from = 0, to = max_height - turbine_height, by = step),
    risk_max = seq(from = turbine_height, to = max_height, by = step)
  )

  # Restrict to shifts within ±40 m of the true turbine position
  if (restrict_bounds) {
    heightshifts <- heightshifts[abs(heightshifts$risk_min - risk_min) <= 40, ]
    rownames(heightshifts) <- NULL
  }

  # Positional index of the true turbine setting in the (possibly restricted) table
  true_fhd_id <- which(heightshifts$risk_min == risk_min)

  # Pre-split data by fhd_id so we only subset the data frame once per FHD
  data_split <- split(fhd_data, fhd_data$fhd_id)
  fhd_ids <- names(data_split)

  # Vectorised computation: for each FHD build a draw × height matrix, then
  # for every height-shift window sum the relevant columns and average over draws.
  hs_risk_min <- heightshifts$risk_min
  hs_risk_max <- heightshifts$risk_max
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
  actual_shifts <- heightshifts$risk_min - risk_min
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
