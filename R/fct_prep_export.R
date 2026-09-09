#' prep_export
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
prep_export <- function(
  fhd_draws,
  type = c("proportion", "pdf")
) {
  # BC NOTE 1 - "pdf" option:
  #
  # I foresee the need to add a "pdf" option to this function to handle FHDs provided
  # as probability density functions (PDFs), for alignment with {stochLAB} requirements.
  #
  # `{ReSCUEtools}` documentation refers to FHD outputs as PDFs, so they will need to be
  # converted into proportions at 1m height bands for export to the {stochLAB}. This will # likely involve additional computation, such as integration over the height bands.
  #
  # The "pdf" options is only a placeholder for now. The function is currently set up to
  # handle FHDs provided as proportions.

  # BC NOTE 2 - "proportion" option:
  #
  # Currently this is a simplified version which assumes all FHDs are provided as
  # proportions at 1m height bands, starting at 0-1m. This is the format required by
  # {stochLAB}.
  #
  # Future versions should include checks and handling of different height band sizes.
  # For now, warnings will be thrown if the FHD deviates from required format.

  type <- match.arg(type)

  # We want to pivot the data to be longer, such that we have one row per height band and one column for each draw
  fhd_crm_format <- fhd_draws |>
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
  fhd_crm_format <- fhd_crm_format |>
    tidyr::pivot_wider(
      names_from = draw_id,
      values_from = probability
    ) |>
    dplyr::ungroup()

  # Handling for CRMs ------------------------------

  if (type == "pdf") {
    # Placeholder for future implementation of PDF handling
    cli::cli_warn(
      "PDF handling is not yet implemented. Returning the input data as-is.",
      class = "warn-fhd-pdf-handling"
    )
    return(fhd_draws)
  } else if (type == "proportion") {
    # Assign height values into 1m intervals, starting from 0-1m, 1-2m, etc. The
    # binned height value represents the lower bound of the interval
    hmax <- 500
    hbins <- cut(
      fhd_crm_format$height,
      # 1m intervals from 0 to hmax
      breaks = seq(0, hmax, by = 1),
      # intervals coded as lower bound
      labels = seq(0, hmax - 1, by = 1),
      include.lowest = TRUE
    ) |>
      # character -> integer needed to get actual numerics from the factor labels
      as.character() |>
      as.integer()

    # get starting bin and bin difference in data
    fhd_bin_start <- min(hbins)
    fhd_bin_diff <- unique(diff(sort(hbins)))

    if (fhd_bin_start != 0) {
      cli::cli_warn(
        "The minimum height band in the FHD is not comprised within the 0-1m interval",
        class = "warn-fhd-hbin-non0min"
      )
    } else if (length(fhd_bin_diff) > 1) {
      # non-uniform band sizes
      cli::cli_warn(
        c(
          "The FHD data has inconsistent height band sizes.",
          i = "These cases might be handled in the future. For now, please provide FHDs with 1m height bands."
        ),
        class = "warn-fhd-hbin-inconsistent"
      )
    } else if (fhd_bin_diff > 1) {
      cli::cli_warn(
        c(
          "Height resolution of FHD is larger than 1m height bands.",
          i = "These cases might be handled in the future. For now, please provide FHDs with a height resolution of 1m."
        ),
        class = "warn-fhd-hbin-non1m"
      )
    } else if (fhd_bin_diff < 1) {
      cli::cli_warn(
        c(
          "Height resolution of FHD is smaller than 1m height bands.",
          i = "These cases might be handled in the future. For now, please provide FHDs with a height resolution of 1m."
        ),
        class = "warn-fhd-hbin-non1m"
      )
    }
  }

  # Return the data in the format required by the {stochLAB} CRM, which is a data frame
  # with one column for each draw and one row for each 1m height band, with the height bands represented as numeric values (lower bound of the interval)
  fhd_crm_format |>
    dplyr::mutate(
      height = as.numeric(as.character(hbins)),
      .keep = "unused"
    )
}
