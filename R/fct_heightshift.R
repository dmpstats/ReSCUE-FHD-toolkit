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
  round = c(NULL, 2)
) {
  # Create a new .df with only the relevant columns
  fhd_data <- data.frame(
    fhd_id = fhd_data[[id_col]],
    height = fhd_data[[height_col]],
    prob = fhd_data[[prob_col]],
    draw_id = fhd_data[[draw_id_col]]
  )

  # Identify the max height in the data
  max_height <- max(fhd_data$height, na.rm = TRUE)
  min_height <- 0

  # Define height_shift_ids as a sequence of shifting heights
  turbine_height <- risk_max - risk_min
  # The highest possible risk_max is the max height in the data (plus 1 for clarity),
  # and the lowest possible risk_min is 0 (the ground level).
  heightshifts <- data.frame(
    risk_min = seq(from = 0, to = max_height - turbine_height, by = 1),
    risk_max = seq(from = turbine_height, to = max_height, by = 1)
  ) |>
    dplyr::mutate(
      height_shift_id = dplyr::row_number()
    )

  # Iterate over the FHDs and calculate the probability of FHD risk for each height shift
  fhd_prob_matrix <- matrix(
    nrow = length(unique(fhd_data$fhd_id)),
    ncol = nrow(heightshifts),
    dimnames = list(unique(fhd_data$fhd_id), heightshifts$height_shift_id)
  )

  for (fid in unique(fhd_data$fhd_id)) {
    for (hsid in heightshifts$height_shift_id) {
      # Get the current risk_min and risk_max for this height shift
      current_risk_min <- heightshifts$risk_min[
        heightshifts$height_shift_id == hsid
      ]
      current_risk_max <- heightshifts$risk_max[
        heightshifts$height_shift_id == hsid
      ]

      # Filter the FHD data for the current FHD and height shift
      fhd_subset <- fhd_data |>
        dplyr::filter(
          fhd_id == fid,
          height >= current_risk_min,
          height <= current_risk_max
        )

      # Considering the draws, calculate the mean probability for the current FHD and height shift
      mean_prob <- fhd_subset |>
        dplyr::group_by(draw_id) |>
        dplyr::summarise(mean_prob = sum(prob, na.rm = TRUE)) |>
        dplyr::summarise(mean_prob = mean(mean_prob, na.rm = TRUE)) |>
        dplyr::pull(mean_prob)
      fhd_prob_matrix[fid, hsid] <- mean_prob
    }
  }

  # Get the ID of the 'true' FHD (where the risk_min is equal to the true risk_min)
  true_fhd_id <- heightshifts |>
    dplyr::ungroup() |>
    dplyr::filter(risk_min == !!risk_min) |>
    dplyr::pull(height_shift_id)

  # Change the column names to + or - the height shift from the true FHD
  colnames(fhd_prob_matrix) <- paste0(
    ifelse(
      heightshifts$height_shift_id < true_fhd_id,
      "-",
      "+"
    ),
    abs(heightshifts$height_shift_id - true_fhd_id),
    "m"
  )

  # If the desired type is percentages, convert all values to
  # percentage change from +0
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

  # Add the FIDs to the LHS of the matrix
  fhd_perc_matrix <- cbind(
    fhd_id = rownames(fhd_perc_matrix),
    fhd_perc_matrix
  )
  fhd_prob_matrix <- cbind(
    fhd_id = rownames(fhd_prob_matrix),
    fhd_prob_matrix
  )

  # Set the column of the true FHD as an attribute
  attr(fhd_prob_matrix, "true_fhd_col") <- true_fhd_id + 1
  attr(fhd_perc_matrix, "true_fhd_col") <- true_fhd_id + 1

  return(
    list(
      prob = fhd_prob_matrix,
      perc = fhd_perc_matrix
    )
  )
}
