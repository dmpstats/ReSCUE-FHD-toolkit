#' Slice and resample an FHD array by covariate levels
#'
#' @description
#' Subsets a 2D or 3D FHD array (as produced by [fhd_df_to_array()]) to a
#' user-selected combination of covariate levels. Accepts both 2D
#' (`n_height × n_draws`) and 3D (`n_height × n_draws × n_covs_combinations`)
#' arrays. The behaviour depends on whether any covariate is dropped:
#'
#' - **No covariates in array** (2D input): returned as-is with no slicing or
#'   resampling.
#' - **All covariates retained**: the array is sliced directly to the requested
#'   covariate-level combinations. Output is 3D. No resampling occurs.
#' - **One or more covariates dropped** (i.e. passed as `NULL` to ...): the effect of
#'   the dropped covariate(s) is marginalised by resampling. For each retained
#'   covariate combination, `n_draws` height profiles are drawn by independently
#'   sampling a random draw index and a random level of the dropped covariate(s)
#'   from the original array. Output is 3D when at least one covariate is
#'   retained, or 2D (`n_height × n_draws`) when all covariates are dropped.
#'
#' @param fhd_array A 2D (`n_height × n_draws`) or 3D
#'   (`n_height × n_draws × n_covs_combinations`) array of class
#'   `fhd_array_from_df`, as produced by [fhd_df_to_array()], carrying `covs`
#'   and `col_names` attributes.
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Named arguments specifying selected
#'   levels for each covariate. Names must match the covariate column names in the
#'   original data. Pass a character vector of levels to retain that covariate, or `NULL`
#'   to drop itcharacter vector of levels to retain that covariate, or `NULL` to drop it
#'   and marginalise over all its levels via resampling. E.g.:
#'   `wind_speed = c("high", "low"), temperature = NULL`. Ignored when
#'   `fhd_array` is 2D (no covariate dimension).
#' @param out_format One of `"df"` (default) or `"array"`. Controls whether the
#'   result is returned as a long-format tibble (via [fhd_array_to_df()]) or as
#'   an array with the same class and attributes as `fhd_array`.
#' @param seed Optional integer seed passed to [set.seed()] before resampling,
#'   for reproducibility. Ignored when no covariate is dropped.
#'
#' @return
#' When `out_format = "array"`: a 2D or 3D array of class `fhd_array_from_df`
#' carrying `covs`, `col_names`, and `fhd` attributes. The shape is:
#' \describe{
#'   \item{3D (`n_height × n_draws × n_retained_cov_combinations`)}{When at
#'   least one covariate is retained in the output.}
#'   \item{2D (`n_height × n_draws`)}{When the input has no covariates, or when
#'   all covariates are dropped via `NULL`.}
#' }
#'
#' When `out_format = "df"`: a long-format tibble equivalent to the array
#' result. ntoto
#'
#' In both cases the `fhd` attribute is a list with:
#' \describe{
#'   \item{`resampled`}{Logical; `TRUE` if any covariate was dropped and
#'   resampling occurred.}
#'   \item{`covs_dropped`}{Character vector of dropped covariate names, or
#'   `NULL` if none were dropped.}
#' }
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
  if (inherits(fhd_array, "fhd_array_from_df") == FALSE) {
    cli::cli_abort(
      "{.arg fhd_array} must be of class {.cls fhd_array_from_df}."
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

    # ndraws in the original FHD array
    ndraws <- dim(fhd_array)[2]

    # cov-levels combos to resample from
    resamp_combos <- expand.grid(
      append(
        cov_picked_levels[!cov_dropped],
        covs$levels[cov_dropped]
      )
    )
    # generate tags for array slicing
    resamp_combos_tags <- do.call(paste, c(resamp_combos, sep = "_"))

    # define multiplier to get required number of resamples, wich is determined by the
    # number of retained covars. This will ensure the output has the specified number of
    # samples for each retained covar combination
    n_covs_retain <- length(covs_retain_name)
    samp_fctr <- if (n_covs_retain > 0) n_covs_retain else 1

    ## resample dim 2 (draws) and dim 3 (covariate combos) independently
    set.seed(seed)
    draws_resamp_idx <- sample(
      dim(fhd_array)[2],
      ndraws * samp_fctr,
      replace = TRUE
    )

    cov_resamp_idx <- match(
      sample(resamp_combos_tags, ndraws * samp_fctr, replace = TRUE),
      dimnames(fhd_array)[[3]]
    )

    # build index matrix: each (draw, cov) pair is repeated across all heights
    n_height <- dim(fhd_array)[1]
    idx <- cbind(
      height = rep(seq_len(n_height), times = ndraws * samp_fctr),
      draw = rep(draws_resamp_idx, each = n_height),
      cov = rep(cov_resamp_idx, each = n_height)
    )

    # single vectorised slicing
    resampled_vals <- fhd_array[idx]

    # reshape to array giiven covar attributes
    retain_combos <- expand.grid(cov_picked_levels[!cov_dropped])
    retain_combos_tags <- do.call(paste, c(retain_combos, sep = "_"))
    n_covs <- if (n_covs_retain > 0) n_covs_retain else NULL

    dim_names <- list(
      height = dimnames(fhd_array)[[1]],
      draw_id = seq_len(ndraws)
    )

    if (!is.null(n_covs)) {
      dim_names$covs <- retain_combos_tags
    }

    sliced_fhd_arr <- array(
      resampled_vals,
      dim = c(n_height, ndraws, n_covs),
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

  # add attribute passing on if fhd was resampled due to covars being dropped from
  # selection
  attr(out, "fhd") <- list(
    resampled = if (any(cov_dropped)) TRUE else FALSE,
    covs_dropped = if (any(cov_dropped)) covs_drop_name else NULL
  )

  return(out)
}
