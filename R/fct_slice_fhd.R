#' Slice and resample an FHD array by covariate levels
#'
#' @description
#' Subsets an N-dimensional FHD array (as produced by [fhd_df_to_array()]) to a
#' user-selected combination of covariate levels. The first two dimensions are
#' always height and draw indices; any remaining dimensions correspond to
#' individual covariates.
#'
#' Behaviour depends on whether any covariate is dropped:
#'
#' - **No covariates in array** (2D input): returned as-is with no slicing or
#'   resampling.
#' - **All covariates retained**: the array is sliced directly to the requested
#'   levels on each covariate dimension. No resampling occurs.
#' - **One or more covariates dropped** (i.e. passed as `NULL` to `...`): the
#'   effect of the dropped covariate(s) is marginalised by resampling. `n_draws`
#'   height profiles are drawn by jointly sampling (without replacement) tuples
#'   of draw index and dropped-covariate level(s) from the original array. The
#'   output has the same number of draws as the input. Output is
#'   `(n_height × n_draws × n_retained_levels_covar_1 × ...)` when at least one covariate
#'   is retained, or 2D (`n_height × n_draws`) when all are dropped.
#'
#' @param fhd_array An N-dimensional array of class `<fhd_array>`, as produced by
#'   [fhd_df_to_array()], carrying `covs` and `col_names` attributes. The first
#'   two dimensions must be height and draw indices respectively; additional
#'   dimensions each represent one covariate.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named arguments specifying
#'   selected levels for each covariate. Names must match the covariate column
#'   names in the original data. Pass a character vector of levels to retain that
#'   covariate, or `NULL` to explicitly drop it and marginalise over all its levels via
#'   resampling. E.g.: `wind_speed = c("high", "low"), temperature = NULL`.
#'   Ignored when `fhd_array` has no covariate dimensions.
#' @param out_format One of `"df"` (default) or `"array"`. Controls whether the
#'   result is returned as a long-format tibble (via [fhd_array_to_df()]) or as
#'   an array of class `fhd_array`.
#' @param seed Optional integer seed passed to [set.seed()] before resampling,
#'   for reproducibility. Ignored when no covariate is dropped.
#'
#' @return
#' In both output formats, the result carries an `fhd` attribute — a list with:
#' \describe{
#'   \item{`resampled`}{Logical; `TRUE` if any covariate was dropped and
#'   resampling occurred.}
#'   \item{`covs_dropped`}{Character vector of dropped covariate names;
#'   `character(0)` if none were dropped.}
#' }
#'
#' When `out_format = "array"`: an array of class `fhd_array` carrying `covs`
#' and `col_names` attributes. Dimensions are:
#' \describe{
#'   \item{`n_height × n_draws × n_retained_levels_covar_1 × ...`}{When at least one
#'   covariate is retained; one dimension per retained covariate.}
#'   \item{`n_height × n_draws`}{When the input has no covariates, or all
#'   covariates are dropped via `NULL`.}
#' }
#'
#' When `out_format = "df"`: a long-format tibble equivalent to the array
#' result, with one row per height × draw × retained-covariate-level combination.
#'
#' @seealso [fhd_df_to_array()], [fhd_array_to_df()]
#'
#' @noRd
slice_fhd <- function(
  fhd_array,
  ...,
  out_format = c("df", "array"),
  seed = NULL
) {
  # input validation ------------------------
  if (inherits(fhd_array, "fhd_array") == FALSE) {
    cli::cli_abort(
      "{.arg fhd_array} must be of class {.cls fhd_array}."
    )
  }

  out_format <- rlang::arg_match(out_format)

  # Get data attributes from original data ------------------------------------------
  covs <- attr(fhd_array, "covs")
  col_names <- attr(fhd_array, "col_names")

  # Early exit when no covariates are present in the FHD array ---------------------
  if (length(col_names$cov_cols) == 0) {
    if (out_format == "df") {
      out <- fhd_array_to_df(fhd_array)
    } else {
      out <- fhd_array
    }
    return(out)
  }

  # Covariate Slicing ---------------------------------------------------------------

  # collect covs picked levels (from UI)
  cov_picked_levels <- rlang::list2(...)

  # ensure the order of covs from UI matches the order in original data
  cov_picked_levels <- cov_picked_levels[names(covs$levels)]

  # logical vector signalling dropped covars, assuming they're are assigned with NULL
  is_cov_dropped <- cov_picked_levels |>
    purrr::map_lgl(~ is.null(.x))

  # track covs status
  covs_picked <- names(is_cov_dropped)[!is_cov_dropped]
  covs_dropped <- names(is_cov_dropped)[is_cov_dropped]

  # identify dims for reference
  dim_names <- names(dimnames(fhd_array))
  draw_dim <- match(col_names$draw_col, dim_names)
  cov_dropped_dims <- match(covs_dropped, dim_names)
  cov_picked_dims <- match(covs_picked, dim_names)

  # if all covariates are being selected, perform straight slicing
  if (length(covs_dropped) == 0) {
    # perform subsetting to selected covariate levels
    sliced_fhd_arr <- abind::asub(
      fhd_array,
      idx = cov_picked_levels,
      dims = cov_picked_dims,
      drop = FALSE
    )

    # assign atributes of the sliced array to reflect selection. This is for subsequent
    # coercing into long-dataframe format. NOTE: covar levels returned in order selected by user, not the original order in the input data.
    attr(sliced_fhd_arr, "covs") <- list(
      levels = cov_picked_levels,
      combos = expand.grid(cov_picked_levels)
    )
    attr(sliced_fhd_arr, "col_names") <- attr(fhd_array, "col_names")

    # assign class for downstream validation
    class(sliced_fhd_arr) <- "fhd_array"
  } else {
    # if some covariates are being dropped, then resampling is needed to convey the effect (and uncertainty) of the deselected covariates on the FHDs
    cli::cli_inform(
      "Resampling FHDs due to dropped covar{?s}: {.val {covs_dropped}}"
    )

    # Step 1: subset original array to levels of picked covariates levels. Dropped
    # covariates are retained for resampling in the next step.
    sliced_fhd_arr <- abind::asub(
      fhd_array,
      idx = cov_picked_levels[!is_cov_dropped],
      dims = cov_picked_dims,
      drop = FALSE
    )

    # Step 2: resample the dropped covariates by sampling a random draw index and a
    # random level of the dropped covariate(s) from the original array.
    set.seed(seed)
    sliced_fhd_arr <- resample_dropped_covars(
      sliced_fhd_arr,
      draw_dim,
      cov_dropped_dims
    )

    # assign atributes of the sliced array to reflect selection. This is for subsequent
    # coercing into long-dataframe format
    attr(sliced_fhd_arr, "covs") <- list(
      levels = cov_picked_levels[!is_cov_dropped],
      combos = expand.grid(cov_picked_levels[!is_cov_dropped])
    )

    attr(sliced_fhd_arr, "col_names") <- list(
      height_col = col_names$height_col,
      draw_col = col_names$draw_col,
      prob_col = col_names$prob_col,
      cov_cols = covs_picked
    )

    # assign class for downstream validation
    class(sliced_fhd_arr) <- "fhd_array"
  }

  if (out_format == "df") {
    out <- fhd_array_to_df(sliced_fhd_arr)
  } else {
    out <- sliced_fhd_arr
  }
  # add attribute passing on if fhd was resampled due to covars being dropped from
  # selection
  attr(out, "fhd") <- list(
    resampled = if (length(covs_dropped) > 0) TRUE else FALSE,
    covs_dropped = covs_dropped
  )

  return(out)
}


resample_dropped_covars <- function(arr, draws_dim, covs_dropped_dims) {
  d <- dim(arr)
  ndim <- length(d)
  other <- setdiff(seq_len(ndim), c(draws_dim, covs_dropped_dims))

  # Permute target dims to front, collapse into one
  perm <- c(draws_dim, covs_dropped_dims, other)
  arr_p <- aperm(arr, perm)
  dp <- dim(arr_p)

  n_target <- prod(dp[seq_along(c(draws_dim, covs_dropped_dims))]) # total tuples
  mat <- matrix(arr_p, nrow = n_target)

  # Sample tuples (rows) jointly
  idx <- sample(n_target, dp[1], replace = FALSE)
  mat <- mat[idx, , drop = FALSE]

  # identify dimnames of dropped covariates
  dropped_covs <- names(dimnames(arr)[covs_dropped_dims])

  # Reshape and permute back, removing the dropped covariates dims
  new_dp <- dp[names(dp) %not_in% dropped_covs]
  out <- aperm(
    array(mat, new_dp),
    order(c(draws_dim, other))
  )

  # Assign dimnames to the output array
  dimnames(out) <- dimnames(arr)[sort(c(draws_dim, other))]

  out
}

# ndraws <- dim(fhd_array)[[draw_dim]]

# # assign atributes of the sliced array to reflect selection. This is for subsequent
# base_levels <- append(
#   list(height = dimnames(fhd_array)[[height_dim]]),
#   cov_picked_levels[!is_cov_dropped]
# ) |>
#   expand.grid(stringsAsFactors = FALSE)

# resamp_combs <- data.frame(
#   draw_id = dimnames(fhd_array)[[draw_dim]],
#   purrr::map(
#     dimnames(fhd_array)[cov_dropped_dims],
#     ~ sample(.x, ndraws, replace = TRUE)
#   )
# )

# subsetting <- dplyr::cross_join(base_levels, resamp_combs)
# subsetting <- subsetting[dim_names]

# # perform subsetting to selected covariate levels
# sliced_fhd_arr <- abind::asub(
#   fhd_array,
#   idx = cov_picked_levels,
#   dims = cov_picked_dims,
#   drop = TRUE
# )

# lapply()

# sample(covs$levels[covs_dropped], 100, replace = TRUE)

# # ndraws in the original FHD array
# ndraws <- dim(fhd_array)[2]

# # cov-levels combos to resample from
# resamp_combos <- expand.grid(
#   append(
#     cov_picked_levels[!cov_dropped],
#     covs$levels[cov_dropped]
#   )
# )
# # generate tags for array slicing
# resamp_combos_tags <- do.call(paste, c(resamp_combos, sep = "_"))

# # define multiplier to get required number of resamples, wich is determined by the
# # number of retained covars. This will ensure the output has the specified number of
# # samples for each retained covar combination
# n_covs_retain <- length(covs_retain_name)
# samp_fctr <- if (n_covs_retain > 0) n_covs_retain else 1

# ## resample dim 2 (draws) and dim 3 (covariate combos) independently
# set.seed(seed)
# draws_resamp_idx <- sample(
#   dim(fhd_array)[2],
#   ndraws * samp_fctr,
#   replace = TRUE
# )

# cov_resamp_idx <- match(
#   sample(resamp_combos_tags, ndraws * samp_fctr, replace = TRUE),
#   dimnames(fhd_array)[[3]]
# )

# # build index matrix: each (draw, cov) pair is repeated across all heights
# n_height <- dim(fhd_array)[1]
# idx <- cbind(
#   height = rep(seq_len(n_height), times = ndraws * samp_fctr),
#   draw = rep(draws_resamp_idx, each = n_height),
#   cov = rep(cov_resamp_idx, each = n_height)
# )

# # single vectorised slicing
# resampled_vals <- fhd_array[idx]

# # reshape to array giiven covar attributes
# retain_combos <- expand.grid(cov_picked_levels[!cov_dropped])
# retain_combos_tags <- do.call(paste, c(retain_combos, sep = "_"))
# n_covs <- if (n_covs_retain > 0) n_covs_retain else NULL

# dim_names <- list(
#   height = dimnames(fhd_array)[[1]],
#   draw_id = seq_len(ndraws)
# )

# if (!is.null(n_covs)) {
#   dim_names$covs <- retain_combos_tags
# }

# sliced_fhd_arr <- array(
#   resampled_vals,
#   dim = c(n_height, ndraws, n_covs),
#   dimnames = dim_names
# )
