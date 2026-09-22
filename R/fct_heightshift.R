#' heightshift
#'
#' @description Evaluate collision risk probabilities across a range of turbine
#' rotor heights for each FHD. This function simulates shifting the baseline turbine rotor
#' vertically through the air column and quantifies the resulting changes in collision
#' probability for each FHD, both as absolute probabilities and as percentage changes
#' relative to the baseline turbine position.
#'
#' @details
#' IMPORTANT: function assumes `fhd_data` contains 1m-band rotor heights, with values
#' representing the centre of the height band (i.e. 0.5, 1.5, 2.5, ..., in meters). There
#' is no formal check for this, as it's an internal function that needs to be quick to
#' run, so ensure your input data is structured correctly.
#'
#' The function operates by:
#' \enumerate{
#'   \item Creating a series of hypothetical rotor positions (CRH windows) within a
#'   selected shift limit (`max_shift`), each maintaining the same rotor diameter
#'   (`risk_max - risk_min`) as the baseline configuration. Shifts extend up to
#'   `max_shift` metres above the baseline and down to the lowest window whose lower
#'   bound is >= 0, in increments of `step_size`.
#'   \item For each shift, computing the probability of FHD risk by aggregating
#'   collision data within the shifted rotor window.
#'   \item Averaging collision probabilities across multiple draws (if present
#'   in the data) to account for uncertainty in FHD encounter positions.
#'   \item Calculating percentage changes relative to the baseline turbine height.
#' }
#'
#' @param fhd_data A data frame containing FHD collision probability data, typically
#' output from an FHD analysis workflow. Expected structure: one row per (FHD, height,
#' draw) combination with associated probability values. Height values must represent the
#' centre of 1m bands.
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
#' @param max_shift Numeric scalar specifying the maximum upward shift (m) applied to
#' the baseline rotor window. Downward shifts extend to the lowest window whose lower
#' bound remains >= 0. Default is `40L`.
#' @param step_size Numeric scalar specifying the increment (m) between successive
#' height shifts. Default is `1L`.
#' @param round A 2-element integer vector `c(prob_places, perc_places)` specifying
#' decimal places for rounding the output. The first element rounds probability values,
#' the second rounds percentage changes. If `NULL`, no rounding is applied.
#' Default is `c(4, 2)`.
#' @param crh_windows Logical. If `TRUE`, the returned list includes an additional
#' `heightshifts` element describing the lower/upper bounds and metre offset of each
#' shifted CRH window. Default is `FALSE`.
#'
#' @return A list with two elements (plus an optional third):
#' \describe{
#' \item{\code{prob}}{A matrix of absolute FHD collision probabilities. Rows represent
#'   FHD identifiers; columns represent height shifts (labeled with meter offsets from
#'   baseline, e.g., `"+0m"`, `"+5m"`, `"-10m"`). The matrix includes
#'   an `fhd_id` column prepended at index 1. An attribute `"bsl_crh_idx"`
#'   marks the column index of the baseline turbine position (offset by 1 for the
#'   `fhd_id` column).}
#'   \item{\code{perc}}{A matrix of percentage changes in FHD collision probability
#'   relative to the baseline configuration. Same structure as `prob`, with values
#'   representing percent change (e.g., `+5` means 5% increase). If the baseline
#'   window falls outside the FHD extent, this matrix is filled with `NA`s.}
#'   \item{\code{heightshifts}}{Only present when `crh_windows = TRUE`. A tibble with
#'   the lower bound (`lw_lim`), upper bound (`up_lim`), and metre offset (`shift`)
#'   of each shifted CRH window.}
#' }
#'
heightshift <- function(
  fhd_data,
  height_col = "height",
  prob_col = "prob",
  id_col = "fhd_id",
  draw_id_col = "draw_id",
  risk_min = 50L,
  risk_max = 100L,
  max_shift = 40L,
  step_size = 1L,
  round = c(4, 2),
  crh_windows = FALSE
) {
  # Pre-processing ----------------------------------------------------------------
  # Create a new .df with only the relevant columns
  fhd_data <- data.frame(
    fhd_id = fhd_data[[id_col]],
    height = fhd_data[[height_col]],
    prob = fhd_data[[prob_col]],
    draw_id = fhd_data[[draw_id_col]]
  )

  fhd_max_height <- max(fhd_data$height, na.rm = TRUE)
  rotor_diameter <- risk_max - risk_min

  # workout the lower bound of the lowest possible shifted CRH window, given step and
  # airgap and height >= 0 contraint
  # 1. number of downward shifts from baseline risk-window
  #n_downsteps <- (floor(risk_min / step_size) * step_size)
  n_downsteps <- floor(risk_min / step_size)
  # 2. lower bound of lowest possible CRH window
  crh_low <- max(
    risk_min - (n_downsteps * step_size),
    risk_min - max_shift
  )

  # compute bounds of shifted CRH windows
  heightshifts <- tibble::tibble(
    lw_lim = seq(
      crh_low,
      risk_min + max_shift,
      by = step_size
    ),
    up_lim = lw_lim + rotor_diameter
  )

  # Positional index of the baseline CRH window in the (possibly restricted) table
  bsl_crh_idx <- which(heightshifts$lw_lim == risk_min)

  # Pre-split data by fhd_id so we only subset the data frame once per FHD
  data_split <- split(fhd_data, fhd_data$fhd_id)
  fhd_ids <- names(data_split)

  # Vectorised computation: for each FHD build a draw × height matrix, then
  # for every height-shift window sum the relevant columns and average over draws.
  hs_risk_min <- heightshifts$lw_lim
  hs_risk_max <- heightshifts$up_lim
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
            return(0)
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
  actual_shifts <- heightshifts$lw_lim - risk_min
  col_labels <- paste0(ifelse(actual_shifts >= 0, "+", ""), actual_shifts, "m")
  dimnames(fhd_prob_matrix) <- list(fhd_ids, col_labels)

  # Percentage change relative to the true turbine position
  fhd_perc_matrix <- sweep(
    fhd_prob_matrix,
    1,
    fhd_prob_matrix[, bsl_crh_idx],
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
  attr(fhd_prob_matrix, "bsl_crh_idx") <- bsl_crh_idx + 1L
  attr(fhd_perc_matrix, "bsl_crh_idx") <- bsl_crh_idx + 1L

  out <- list(
    prob = fhd_prob_matrix,
    perc = fhd_perc_matrix
  )

  if (crh_windows) {
    heightshifts$shift <- actual_shifts
    out$heightshifts <- heightshifts
  }

  return(out)
}
