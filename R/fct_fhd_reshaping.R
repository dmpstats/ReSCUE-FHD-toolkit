#' Reshape a long-format FHD dataframe into a 3D array
#'
#' @param data A dataframe with one row per height x draw x covariate-level combination.
#' @param height_col Name of the height column.
#' @param draw_col Name of the draw ID column.
#' @param prob_col Name of the probability (value) column.
#'
#' @return A 3D array with dimensions `n_height x n_draws x n_covs_combinations`.
#'   Dimnames are `height` (sorted height values), `draw_id` (sorted draw IDs),
#'   and `covs` (covariate combinations, sorted and pasted together with "_").
#'
#' @importFrom dplyr across all_of
fhd_df_to_array <- function(
  data,
  height_col = "height",
  draw_col = "draw_id",
  prob_col = "probability"
  #cov_cols
) {
  base_cols <- c(draw_col, height_col, prob_col)
  missing_cols <- setdiff(base_cols, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "Required column(s) not found in `data`: {.val {missing_cols}}"
    )
  }

  # Identify covariate columns, assumed as all remaining columns
  # Distinct combinations of these columns become the third array dimension.
  cov_cols <- setdiff(names(data), base_cols)

  height_vals <- sort(unique(data[[height_col]]))
  draw_vals <- sort(unique(data[[draw_col]]))

  # Distinct, sorted covariate combinations
  cov_combos <- data |>
    dplyr::distinct(across(all_of(cov_cols))) |>
    dplyr::arrange(across(all_of(cov_cols)))

  # get levels for each covariate
  cov_levels <- lapply(cov_combos, function(x) sort(unique(x)))

  n_height <- length(height_vals)
  n_draws <- length(draw_vals)

  # Full grid skeleton so any missing combinations become explicit NAs
  # rather than silently misaligning values in the array
  skeleton <- do.call(
    tidyr::expand_grid,
    setNames(list(height_vals, draw_vals), c(height_col, draw_col))
  ) |>
    dplyr::cross_join(cov_combos)

  filled <- skeleton |>
    dplyr::left_join(data, by = c(height_col, draw_col, cov_cols)) |>
    dplyr::arrange(across(all_of(c(rev(cov_cols), draw_col, height_col))))

  if (anyNA(filled[[prob_col]])) {
    warning(
      "`data` is missing some combinations of ",
      height_col,
      ", ",
      draw_col,
      " and ",
      paste(cov_cols, collapse = ", "),
      "; corresponding array entries are set to NA."
    )
  }

  # parse to array with dimensions height x draw x cov1 X cov2 X ...
  result <- array(
    filled[[prob_col]],
    dim = c(
      n_height = n_height,
      n_draws = n_draws,
      lengths(cov_levels)
    ),
    dimnames = list(
      height = height_vals,
      draw_id = draw_vals
    ) |>
      append(cov_levels)
  )

  # drop last dimension in the absence of covars
  if (length(cov_cols) == 0) {
    result <- result[,, drop = TRUE]
  }

  # Keep the original covariate combinations and column names as
  # attributes, so `fhd_array_to_df()` can round-trip with minor attrition
  attr(result, "covs") <- list(
    levels = cov_levels,
    combos = cov_combos
  )
  attr(result, "col_names") <- list(
    height_col = height_col,
    draw_col = draw_col,
    prob_col = prob_col,
    cov_cols = cov_cols
  )

  # Assign a class for instant validation in `slice_fhd()`
  class(result) <- "fhd_array"

  return(result)
}


#' Convert a multidimensional FHD array back to a long-format dataframe
#'
#' Inverse of [fhd_df_to_array()]. Relies on the `covs` and `col_names`
#' attributes attached by [fhd_df_to_array()] to recover the original
#' covariate columns and column names
#'
#' @param arr A 3D array produced by [fhd_df_to_array()], with dimensions
#'   `n_height x n_draws x n_covs_combinations`.
#' @param height_col,draw_col,prob_col Optional overrides for the height,
#'   draw ID and probability column names in the output dataframe. Default to
#'   the names used when `arr` was created via [fhd_df_to_array()].
#'
#' @return A long-format tibble with one row per height x draw x
#'   covariate-level combination: `height_col`, `draw_col`, the original
#'   covariate columns, and `prob_col`.
#'
#' @importFrom rlang !!! :=
fhd_array_to_df <- function(
  arr,
  height_col = NULL,
  draw_col = NULL,
  prob_col = NULL
) {
  if (!inherits(arr, "fhd_array")) {
    cli::cli_abort(
      "`arr` must be produced by `fhd_df_to_array()` to be converted back to a dataframe format."
    )
  }

  covs <- attr(arr, "covs")
  col_names <- attr(arr, "col_names")

  # sort main col names to match the original input, if not explicitly overridden
  height_col <- height_col %||% col_names$height_col
  draw_col <- draw_col %||% col_names$draw_col
  prob_col <- prob_col %||% col_names$prob_col

  dims <- dim(arr)

  # check that the array has more than 2 dimensions if covariates are present
  if (length(covs$levels) > 1) {
    if (!length(dims) > 2) {
      cli::cli_abort(
        "`arr` must have more than 2 dimensions, as covariates are expected."
      )
    }
  }

  # get values for height and draw from the dimnames, converting to numeric
  dn <- dimnames(arr)
  height_vals <- as.numeric(dn[[1]])
  draw_vals <- type.convert(dn[[2]], as.is = TRUE)

  # Array flattens with height fastest-varying, then draw, then covariate combinations
  # (dim 1, 2, 3, ... respectively) -- rebuild by grid expansion with first column
  # varying fastest for correct matching.
  out <- tidyr::expand_grid(
    {{ height_col }} := height_vals,
    {{ draw_col }} := draw_vals,
    !!!covs$levels,
    .vary = "fastest"
  )

  # add flat probability values from the array
  out[[prob_col]] <- as.vector(arr)

  out
}
