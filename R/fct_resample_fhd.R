#' resample_fhd
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
slice_fhd <- function(
  fhd_array,
  #cov_selected_levels = NULL,
  ...,
  out_format = c("df", "array"),
  n_resamp = 10,
  seed = NULL
) {
  # input validation ------------------------
  if (inherits(fhd_array, "fhd_array_from_df") == FALSE) {
    cli::cli_abort(
      "{.arg fhd_array} must be of class {.cls fhd_array_from_df}."
    )
  }

  out_format <- rlang::arg_match(out_format)

  # Data attributes from original data ------------------------------------------
  covs <- attr(fhd_array, "covs")
  col_names <- attr(fhd_array, "col_names")

  # collect covs picked levels (from UI)
  cov_picked_levels <- rlang::list2(...)

  # ensure the order of covs from UI matches the order in original data
  cov_picked_levels <- cov_picked_levels[names(covs$levels)]

  # logical vector signalling dropped covars, assuming they're are assigned with NULL
  cov_dropped <- cov_picked_levels |>
    purrr::map_lgl(~ is.null(.x))

  # track covs status
  covs_retain_name <- names(cov_dropped)[!cov_dropped]
  covs_drop_name <- names(cov_dropped)[cov_dropped]

  # if all covariates are being selected, perform straight slicing
  if (!any(cov_dropped)) {
    # selected combos
    slc_combos <- expand.grid(cov_picked_levels)
    # generate tags for array slicing
    slc_combos_tags <- do.call(paste, c(slc_combos, sep = "_"))

    # slice original array to selected cov combos
    sliced_fhd_arr <- fhd_array[,, slc_combos_tags, drop = FALSE]

    # assign atributes of the sliced array to reflect selection. This is for subsequent
    # coercing into long-dataframe format
    attr(sliced_fhd_arr, "covs") <- list(
      levels = cov_picked_levels,
      combos = slc_combos
    )
    attr(sliced_fhd_arr, "col_names") <- attr(fhd_array, "col_names")
  } else {
    # if some covariates are being dropped, then resampling is needed to convey the effect (and uncertainty) of the dropped covariates on the FHDs
    cli::cli_inform(
      "Resampling FHDs due to dropped covar{?s}: {.val {covs_drop_name}}"
    )

    # cov-levels combos to resample from
    resamp_combos <- expand.grid(
      append(
        cov_picked_levels[!cov_dropped],
        covs$levels[cov_dropped]
      )
    )
    # generate tags for array slicing
    resamp_combos_tags <- do.call(paste, c(resamp_combos, sep = "_"))

    # # define real number of resamples, wich is dependent on number of retained covars
    # n_covs_retain <- length(covs_retain_name)
    # n_resamp <- max(n_resamp * n_covs_retain, 1)

    # resample dim 2 (draws) and dim 3 (covariate combos) independently
    draws_resamp_idx <- sample(dim(fhd_array)[2], n_resamp, replace = TRUE)
    cov_resamp_idx <- match(
      sample(resamp_combos_tags, n_resamp, replace = TRUE),
      dimnames(fhd_array)[[3]]
    )

    # build index matrix: each (draw, cov) pair is repeated across all heights
    n_height <- dim(fhd_array)[1]
    idx <- cbind(
      height = rep(seq_len(n_height), times = n_resamp),
      draw = rep(draws_resamp_idx, each = n_height),
      cov = rep(cov_resamp_idx, each = n_height)
    )

    # single vectorised slicing
    resampled_vals <- fhd_array[idx]

    # reshape to array giiven cov attributes
    retain_combos <- expand.grid(cov_picked_levels[!cov_dropped])
    retain_combos_tags <- do.call(paste, c(retain_combos, sep = "_"))
    n_covs <- if (length(retain_combos_tags) > 0) {
      length(retain_combos_tags)
    } else {
      NULL
    }

    dim_names <- list(
      height = dimnames(fhd_array)[[1]],
      draw_id = seq_len(n_resamp)
    )

    if (!is.null(n_covs)) {
      dim_names$covs <- retain_combos_tags
    }

    sliced_fhd_arr <- array(
      resampled_vals,
      dim = c(n_height, n_resamp, n_covs),
      dimnames = dim_names
    )

    # assign atributes of the sliced array to reflect selection. This is for subsequent
    # coercing into long-dataframe format
    attr(sliced_fhd_arr, "covs") <- list(
      levels = cov_picked_levels[!cov_dropped],
      combos = retain_combos
    )

    attr(sliced_fhd_arr, "col_names") <- list(
      height_col = col_names$height_col,
      draw_col = col_names$draw_col,
      prob_col = col_names$prob_col,
      cov_cols = covs_retain_name
    )
  }

  if (out_format == "df") {
    out <- fhd_array_to_df(sliced_fhd_arr)
  } else {
    out <- sliced_fhd_arr
  }

  # pass on if fhd was resampled due to covars being dropped from selection
  attr(out, "fhd") <- list(
    resampled = if (any(cov_dropped)) TRUE else FALSE,
    covs_dropped = if (any(cov_dropped)) covs_drop_name else NULL
  )

  return(out)
}


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
      "Column(s) not found in `data`: {.val {missing_cols}}"
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
  result
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
  if (is.null(dims) || length(dims) != 3) {
    stop("`arr` must be a 3D array, as produced by `fhd_df_to_array()`.")
  }
  n_height <- dims[[1]]
  n_draws <- dims[[2]]
  n_combos <- dims[[3]]

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
