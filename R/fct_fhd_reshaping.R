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

  # Distinct, sorted covariate combinations -> single "combo" dimension
  cov_combos <- data |>
    dplyr::distinct(across(all_of(cov_cols))) |>
    dplyr::arrange(across(all_of(cov_cols)))

  # get levels for each covariate
  cov_levels <- lapply(cov_combos, function(x) sort(unique(x)))

  cov_labels <- cov_combos |>
    tidyr::unite("cov_comb", everything(), sep = "_") |>
    dplyr::pull("cov_comb")

  n_height <- length(height_vals)
  n_draws <- length(draw_vals)
  n_combos <- nrow(cov_combos)

  # Full grid skeleton so any missing combinations become explicit NAs
  # rather than silently misaligning values in the array
  skeleton <- do.call(
    tidyr::expand_grid,
    setNames(list(height_vals, draw_vals), c(height_col, draw_col))
  ) |>
    dplyr::cross_join(cov_combos)

  filled <- skeleton |>
    dplyr::left_join(data, by = c(height_col, draw_col, cov_cols)) |>
    dplyr::arrange(across(all_of(c(cov_cols, draw_col, height_col))))

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

  result <- array(
    filled[[prob_col]],
    dim = c(
      n_height = n_height,
      n_draws = n_draws,
      n_covs_combinations = n_combos
    ),
    dimnames = list(
      height = height_vals,
      draw_id = draw_vals,
      covs = cov_labels
    )
  )

  # drop last dimension in the absence of covars
  if (length(cov_cols) == 0) {
    result <- result[,, 1, drop = TRUE]
  }

  # Keep the original (unpasted) covariate combinations and column names as
  # attributes, so `fhd_array_to_df()` can round-trip without having to parse
  # the pasted `covs` dimnames (which would be ambiguous if a covariate value
  # contains "_", or if there is more than one covariate column).
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

  # Assign a class for instant validation in `resample_fhd()`
  class(result) <- "fhd_array_from_df"

  return(result)
}


#' Convert a 3D FHD array back to a long-format dataframe
#'
#' Inverse of [fhd_df_to_array()]. Relies on the `cov_combos` and `col_names`
#' attributes attached by [fhd_df_to_array()] to recover the original
#' covariate columns and column names, rather than parsing the pasted `covs`
#' dimnames.
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
fhd_array_to_df <- function(arr) {
  covs <- attr(arr, "covs")
  col_names <- attr(arr, "col_names")

  if (is.null(covs) || is.null(col_names)) {
    stop(
      "`arr` is missing `covs`/`col_names` attributes; it must be ",
      "produced by `fhd_df_to_array()` to be converted back with ",
      "`fhd_array_to_df()`."
    )
  }

  # isolate combos
  cov_combos <- covs$combos

  height_col <- col_names$height_col
  draw_col <- col_names$draw_col
  prob_col <- col_names$prob_col

  dims <- dim(arr)

  # check for 3D array if covars are present
  if (ncol(cov_combos) > 0) {
    if (is.null(dims) || length(dims) != 3) {
      cli::cli_abort(
        "`arr` must be a 3D array, as produced by `fhd_df_to_array()`."
      )
    }
  }

  n_height <- dims[[1]]
  n_draws <- dims[[2]]
  n_combos <- if (length(dims) == 3) dims[[3]] else 1
  #n_combos <- dims[[3]]

  dn <- dimnames(arr)
  height_vals <- as.numeric(dn[[1]])
  draw_vals <- type.convert(dn[[2]], as.is = TRUE)

  # Array flattens with height fastest-varying, then draw, then covariate
  # combination (dim 1, 2, 3 respectively) -- rebuild each column to match.
  cov_rows <- cov_combos[
    rep(seq_len(n_combos), each = n_height * n_draws),
    ,
    drop = FALSE
  ]

  out <- cov_rows
  out[[draw_col]] <- rep(draw_vals, times = n_combos, each = n_height)
  out[[height_col]] <- rep(height_vals, times = n_draws * n_combos)
  out[[prob_col]] <- as.vector(arr)

  tibble::as_tibble(out[c(height_col, draw_col, names(cov_combos), prob_col)])
}
