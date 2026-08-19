# Small, hand-verifiable fixture: 2 heights x 5 draws, with up to 3 covariates
# (wind_speed x temperature x precipitation). Use n_covs to select how many to
# include (0–3). No covariate is a scenario.
make_fhd_df <- function(n_covs = 3L) {
  covariates <- list(
    wind_speed = c("high", "low"),
    temperature = c("hot", "mild", "cold"),
    precipitation = c("snow", "none")
  )

  if (!n_covs %in% 0:3) {
    stop("`n_covs` must be an integer between 0 and 3.")
  }

  base <- list(
    draw_id = 1:5,
    height = seq(0.5, 1.5, by = 1)
  )

  grid_args <- c(base, covariates[seq_len(n_covs)])

  do.call(expand.grid, grid_args) |>
    dplyr::mutate(
      probability = runif(dplyr::n(), 1, 5),
      probability = probability / sum(probability),
      .by = draw_id
    )
}

# options(dplyr.print_max = Inf)

# fhd_df_to_array() ---------------------------------------------------------------------

test_that("fhd_df_to_array() produces correct dimensions and dimnames", {
  arr <- fhd_df_to_array(make_fhd_df())

  expect_equal(
    dim(arr),
    c(
      n_height = 2L,
      n_draws = 5L,
      wind_speed = 2L,
      temperature = 3L,
      precipitation = 2L
    )
  )
  expect_named(
    dimnames(arr),
    c("height", "draw_id", "wind_speed", "temperature", "precipitation")
  )
  expect_equal(dimnames(arr)$height, c("0.5", "1.5"))
  expect_equal(dimnames(arr)$draw_id, c("1", "2", "3", "4", "5"))
  expect_equal(dimnames(arr)$wind_speed, c("high", "low"))
  expect_equal(dimnames(arr)$temperature, c("hot", "mild", "cold"))
  expect_equal(dimnames(arr)$precipitation, c("snow", "none"))
})


test_that("fhd_df_to_array() places values in the correct cells", {
  # 3-covars
  fhd_df <- make_fhd_df()
  fhd_arr <- fhd_df_to_array(fhd_df)

  expect_equal(
    as.numeric(fhd_arr["0.5", , "high", "hot", "none"]),
    fhd_df |>
      dplyr::filter(
        height == 0.5,
        wind_speed == "high",
        temperature == "hot",
        precipitation == "none"
      ) |>
      dplyr::pull(probability)
  )

  expect_equal(
    as.numeric(fhd_arr["1.5", , "low", "hot", "snow"]),
    fhd_df |>
      dplyr::filter(
        height == 1.5,
        wind_speed == "low",
        temperature == "hot",
        precipitation == "snow"
      ) |>
      dplyr::pull(probability)
  )

  # 2-covars
  fhd_df <- make_fhd_df(n_covs = 2)
  fhd_arr <- fhd_df_to_array(fhd_df)

  expect_equal(
    as.numeric(fhd_arr["0.5", , "low", "mild"]),
    fhd_df |>
      dplyr::filter(
        height == 0.5,
        wind_speed == "low",
        temperature == "mild",
      ) |>
      dplyr::pull(probability)
  )

  expect_equal(
    as.numeric(fhd_arr[, "2", "high", "cold"]),
    fhd_df |>
      dplyr::filter(
        draw_id == 2,
        wind_speed == "high",
        temperature == "cold",
      ) |>
      dplyr::pull(probability)
  )

  # No covars
  fhd_df <- make_fhd_df(n_covs = 0)
  fhd_arr <- fhd_df_to_array(fhd_df)

  expect_equal(
    as.numeric(fhd_arr["0.5", ]),
    fhd_df |>
      dplyr::filter(height == 0.5) |>
      dplyr::pull(probability)
  )
})


test_that("fhd_df_to_array() warns and fills NA for missing combinations", {
  df <- make_fhd_df(n_covs = 1)[-8, ] # drop (height=1.5, draw=3, wind_speed="high")

  expect_warning(
    arr <- fhd_df_to_array(df),
    "missing some combinations"
  )

  expect_equal(sum(is.na(arr)), 1L)
  expect_true(is.na(arr["1.5", "3", "high"]))
  # unaffected cells stay intact
  expect_equal(
    arr["1.5", "1", "high"],
    df[6, "probability"]
  )
})


test_that("fhd_df_to_array() attaches cov_combos and col_names attributes", {
  arr <- fhd_df_to_array(make_fhd_df(n_covs = 1))

  expect_equal(
    attr(arr, "covs")$levels,
    list(wind_speed = factor(c("high", "low")))
  )

  expect_equal(
    attr(arr, "col_names"),
    list(
      height_col = "height",
      draw_col = "draw_id",
      prob_col = "probability",
      cov_cols = "wind_speed"
    )
  )
})


test_that("fhd_df_to_array() handles covar-free FHD", {
  # data without covariates: 2 heights x 2 draws, forming a complete grid with known probability values.
  fhd_df <- expand.grid(
    draw_id = 1:5,
    height = seq(0.5, 2.5, by = 1)
  ) |>
    dplyr::mutate(probability = runif(length(draw_id)))

  arr <- fhd_df_to_array(fhd_df)

  expect_equal(
    dim(arr),
    c(n_height = 3L, n_draws = 5L)
  )

  expect_named(dimnames(arr), c("height", "draw_id"))
  expect_equal(dimnames(arr)$height, c("0.5", "1.5", "2.5"))
  expect_equal(dimnames(arr)$draw_id, c("1", "2", "3", "4", "5"))
})


# fhd_array_to_df() ---------------------------------------------------------------------
test_that("fhd_array_to_df() round-trips a single-covariate array exactly", {
  df <- make_fhd_df(n_covs = 1)
  arr <- fhd_df_to_array(df)
  df_back <- fhd_array_to_df(arr)

  expect_setequal(names(df_back), names(df))

  df_sorted <- dplyr::arrange(df, wind_speed, draw_id, height)
  df_back_sorted <- dplyr::arrange(df_back, wind_speed, draw_id, height)

  expect_equal(df_back_sorted$probability, df_sorted$probability)
  expect_equal(df_back_sorted$height, df_sorted$height)
  expect_equal(df_back_sorted$draw_id, df_sorted$draw_id)
  expect_equal(df_back_sorted$wind_speed, df_sorted$wind_speed)
})


test_that("fhd_array_to_df() round-trips a multi-covariate array exactly", {
  df <- make_fhd_df(n_covs = 2)
  arr <- fhd_df_to_array(df)
  df_back <- fhd_array_to_df(arr)

  df_sorted <- df |>
    dplyr::arrange(wind_speed, temperature, draw_id, height)

  df_back_sorted <- df_back |>
    dplyr::arrange(wind_speed, temperature, draw_id, height)

  expect_equal(df_back_sorted$probability, df_sorted$probability)
  expect_equal(df_back_sorted$temperature, df_sorted$temperature)
  expect_equal(df_back_sorted$wind_speed, df_sorted$wind_speed)
})

test_that("fhd_array_to_df() handles covar-free case", {
  # data without covariates: 2 heights x 2 draws, forming a complete grid with known probability values.
  fhd_df <- expand.grid(
    draw_id = 1:5,
    height = seq(0.5, 2.5, by = 1)
  ) |>
    dplyr::mutate(probability = runif(length(draw_id)))

  arr <- fhd_df_to_array(fhd_df)
  df_back <- fhd_array_to_df(arr)

  expect_setequal(names(df_back), names(fhd_df))

  df_sorted <- dplyr::arrange(fhd_df, draw_id, height)
  df_back_sorted <- dplyr::arrange(df_back, draw_id, height)

  expect_equal(df_back_sorted$probability, df_sorted$probability)
  expect_equal(df_back_sorted$height, df_sorted$height)
})


test_that("fhd_array_to_df() respects column name overrides", {
  arr <- fhd_df_to_array(make_fhd_df(n_covs = 1))
  df_back <- fhd_array_to_df(
    arr,
    height_col = "flight_height",
    draw_col = "draw",
    prob_col = "prob"
  )

  expect_setequal(
    names(df_back),
    c("flight_height", "draw", "wind_speed", "prob")
  )
})


test_that("fhd_array_to_df() errors when array lacks required attributes", {
  plain_arr <- array(1:8, dim = c(2, 2, 2))

  expect_error(
    fhd_array_to_df(plain_arr),
    "must be produced by"
  )
})

# test_that("fhd_array_to_df() errors when array is not 3-dimensional", {
#   arr <- fhd_df_to_array(make_fhd_df())
#   matrix_arr <- arr[,, 1] # drop to 2D, keeping cov_combos/col_names attributes
#   attr(matrix_arr, "cov_combos") <- attr(arr, "cov_combos")
#   attr(matrix_arr, "col_names") <- attr(arr, "col_names")

#   expect_error(
#     fhd_array_to_df(matrix_arr),
#     "3D array"
#   )
# })
