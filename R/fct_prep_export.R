#' prep_export
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
prep_export <- function(
  fhd_draws
) {
  # We want to pivot the data to be longer, such that we have one row per height band and one column for each draw
  out_data <- fhd_draws |>
    dplyr::select(
      height,
      draw_id,
      probability
    ) |>
    dplyr::group_by(draw_id) |>
    dplyr::mutate(
      draw_id = paste0("bootld_", draw_id)
    )
  # Now, we want one column per draw_id, with the associated heights as rows
  out_data <- out_data |>
    tidyr::pivot_wider(
      names_from = draw_id,
      values_from = probability
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(
      height_m = height
    )
}
