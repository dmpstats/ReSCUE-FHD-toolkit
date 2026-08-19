# Small, hand-verifiable fixture: 2 heights x 5 draws, with up to 3 covariates
# (wind_speed x temperature x precipitation). Use n_covs to select how many to
# include (0–3). No covariate is a scenario.
mock_fhd_df <- function(n_covs = 3L) {
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

  expand.grid(
    c(base, covariates[seq_len(n_covs)])
  ) |>
    dplyr::mutate(
      probability = runif(dplyr::n(), 1, 5),
      probability = probability / sum(probability),
      .by = draw_id
    )
}


# 2D input (no covariates) ----------------------------------------------------

test_that("slice_fhd() passes through a covariate-free 2D array unchanged (early exit)", {
  fhd_df <- mock_fhd_df(n_covs = 0)
  arr_2d <- fhd_df_to_array(fhd_df)

  out <- slice_fhd(arr_2d, out_format = "array")
  expect_equal(dim(out), dim(arr_2d))
  expect_equal(out, arr_2d)
  expect_false(attr(out, "fhd")$resampled %||% FALSE)

  out <- slice_fhd(arr_2d, out_format = "df")
  expect_equal(dim(out), dim(fhd_df))

  expect_equal(
    dplyr::arrange(out, height, draw_id)$probability,
    dplyr::arrange(fhd_df, height, draw_id)$probability
  )
})


# Direct slice (no covariates dropped) ----------------------------------------

test_that("slice_fhd() slices to single covariate levels correctly", {
  fhd_df <- mock_fhd_df(n_covs = 2)
  fhd_arr <- fhd_df_to_array(fhd_df)

  out <- slice_fhd(
    fhd_arr,
    wind_speed = "high",
    temperature = "hot",
    out_format = "array"
  )

  expect_equal(dim(out), c(2L, 5L, 1L, 1L), ignore_attr = TRUE)
  expect_equal(dimnames(out)$wind_speed, "high")
  expect_equal(dimnames(out)$temperature, "hot")
  # values must match the corresponding slice of the original array
  expect_equal(out[,, 1L, 1L], fhd_arr[,, "high", "hot"])
  expect_false(attr(out, "fhd")$resampled)
  expect_length(attr(out, "fhd")$covs_dropped, 0L)

  # different covs levels - compare sliced values with original df
  out <- slice_fhd(
    fhd_arr,
    temperature = "hot",
    wind_speed = "low",
    out_format = "array"
  )

  expect_equal(
    as.numeric(out["0.5", , , ]),
    fhd_df |>
      dplyr::filter(
        height == 0.5,
        wind_speed == "low",
        temperature == "hot"
      ) |>
      dplyr::pull(probability)
  )
})


test_that("slice_fhd() slices to multiple covariate levels across covars", {
  fhd_df <- mock_fhd_df(n_covs = 3)
  fhd_arr <- fhd_df_to_array(fhd_df)

  # array output
  out <- slice_fhd(
    fhd_arr,
    wind_speed = "high",
    temperature = c("cold", "hot"),
    precipitation = "none",
    out_format = "array"
  )

  expect_equal(dim(out), c(2L, 5L, 1L, 2L, 1L), ignore_attr = TRUE)
  expect_equal(dimnames(out)$wind_speed, "high")
  expect_equal(dimnames(out)$temperature, c("cold", "hot"))
  # values must match the corresponding slice of the original array
  expect_equal(out[,, 1L, 1L, 1L], fhd_arr[,, "high", "cold", "none"])
  expect_equal(out[,, 1L, 2L, 1L], fhd_arr[,, "high", "hot", "none"])
  expect_false(attr(out, "fhd")$resampled)
  expect_length(attr(out, "fhd")$covs_dropped, 0)

  expect_equal(
    as.numeric(out[, "3", , , ]), # draw 3
    fhd_df |>
      dplyr::filter(
        draw_id == 3,
        wind_speed == "high",
        precipitation == "none",
        temperature %in% c("cold", "hot")
      ) |>
      dplyr::arrange(dplyr::desc(temperature)) |>
      dplyr::pull(probability)
  )

  # df output
  out <- slice_fhd(
    fhd_arr,
    wind_speed = c("low", "high"),
    temperature = c("mild"),
    precipitation = "snow",
    out_format = "df"
  )

  expect_equal(
    out,
    fhd_df |>
      dplyr::filter(,
        wind_speed %in% c("low", "high"),
        temperature == "mild",
        precipitation == "snow"
      ) |>
      dplyr::relocate(height, .before = draw_id) |>
      dplyr::arrange(
        precipitation,
        temperature,
        desc(wind_speed),
        draw_id,
        height
      ) |>
      tibble::as_tibble() |>
      dplyr::mutate(across(where(is.factor), as.character)),
    ignore_attr = TRUE
  )
})


# Resampling (covariates dropped) ---------------------------------------------

test_that("slice_fhd() resamples when one covariate is dropped", {
  # 2 initial covariates
  fhd_df <- mock_fhd_df(n_covs = 2)
  fhd_arr <- fhd_df_to_array(fhd_df)

  # drop temperature, retain one level of wind_speed
  out <- slice_fhd(
    fhd_arr,
    wind_speed = c("low"),
    temperature = NULL,
    seed = 42,
    out_format = "array"
  )
  # one retained covariate level -> one output slice
  expect_equal(dim(out), c(2L, 5L, 1L), ignore_attr = TRUE)
  expect_equal(dimnames(out)$wind_speed, "low")
  expect_false(anyNA(out))
  expect_true(attr(out, "fhd")$resampled)
  expect_equal(attr(out, "fhd")$covs_dropped, "temperature")

  expect_in(
    out[, "3", "low"],
    fhd_arr[,, "low", ]
  )

  # drop temperature, retain two levels of wind_speed
  out <- slice_fhd(
    fhd_arr,
    wind_speed = c("low", "high"),
    temperature = NULL,
    seed = 42,
    out_format = "array"
  )

  expect_equal(dim(out), c(2L, 5L, 2L), ignore_attr = TRUE)
  expect_setequal(dimnames(out)$wind_speed, c("low", "high"))
  expect_equal(attr(out, "fhd")$covs_dropped, "temperature")

  # inspect values - output values must be contained in the original array, as they are sampled from a random draw of the dropped covariate
  expect_in(out[, "1", "low"], fhd_arr[,, "low", ])
  expect_in(out[, "5", "high"], fhd_arr[,, "high", ])
})


test_that("slice_fhd() resamples when two of three covariates are dropped", {
  # 3 initial covariates
  fhd_df <- mock_fhd_df(n_covs = 3)
  fhd_arr <- fhd_df_to_array(fhd_df)

  # drop windspeed and temperature, retain both levels of precipitation
  out <- slice_fhd(
    fhd_arr,
    wind_speed = NULL,
    temperature = NULL,
    precipitation = c("snow", "none"),
    seed = 42,
    out_format = "array"
  )

  # retained covariate with 2 levels
  expect_equal(dim(out), c(2L, 5L, 2L), ignore_attr = TRUE)
  expect_equal(dimnames(out)$precipitation, c("snow", "none"))
  expect_null(dimnames(out)$wind_speed)
  expect_null(dimnames(out)$temperature)
  expect_equal(attr(out, "fhd")$covs_dropped, c("wind_speed", "temperature"))

  # inspect values - output values must be contained in the original array
  expect_in(out[, "2", "snow"], fhd_arr[,,,, "snow"])
  expect_in(out[, "5", "none"], fhd_arr[,,,, "none"])
})


test_that("slice_fhd() resamples when single covariate is dropped", {
  # 1 initial covariates
  fhd_df <- mock_fhd_df(n_covs = 1)
  fhd_arr <- fhd_df_to_array(fhd_df)

  # drop temperature, retain one level of wind_speed
  out <- slice_fhd(
    fhd_arr,
    wind_speed = NULL,
    seed = 22,
    out_format = "array"
  )
  # only covar is dropped -> 2D output
  expect_equal(dim(out), c(2L, 5L), ignore_attr = TRUE)
  expect_null(dimnames(out)$wind_speed)
  expect_true(attr(out, "fhd")$resampled)
  expect_equal(attr(out, "fhd")$covs_dropped, "wind_speed")

  # inspect values - output values must be contained in the original array, as they are
  expect_in(
    out[, "3"],
    fhd_arr[,,]
  )
})


test_that("slice_fhd() produces a 2D array when all covariates are dropped", {
  # 3 initial covariates
  fhd_df <- mock_fhd_df(n_covs = 3)
  fhd_arr <- fhd_df_to_array(fhd_df)

  out <- slice_fhd(
    fhd_arr,
    wind_speed = NULL,
    temperature = NULL,
    precipitation = NULL,
    seed = 99,
    out_format = "array"
  )

  expect_equal(length(dim(out)), 2L)
  expect_equal(dim(out)[[1L]], dim(fhd_arr)[[1L]]) # heights unchanged
  expect_equal(dim(out)[[2L]], dim(fhd_arr)[[2L]]) # same ndraws
  expect_false(anyNA(out))
  expect_true(attr(out, "fhd")$resampled)
  expect_setequal(
    attr(out, "fhd")$covs_dropped,
    c("wind_speed", "temperature", "precipitation")
  )
})

# test_that("slice_fhd() data.frame output is as expected", {
#   # 2 initial covariates
#   fhd_df <- mock_fhd_df(n_covs = 2)
#   fhd_arr <- fhd_df_to_array(fhd_df)

#   # drop wind_speed, return for 2 levels of temperature
#   out <- slice_fhd(
#     fhd_arr,
#     wind_speed = NULL,
#     temperature = c("mild", "cold"),
#     seed = 99,
#     out_format = "df"
#   )

#   fhd_df_comp <- fhd_df |>
#     dplyr::filter(
#       temperature %in% c("mild", "cold")
#     ) |>
#     dplyr::relocate(height, .before = draw_id) |>
#     dplyr::arrange(
#       temperature,
#       wind_speed,
#       draw_id,
#       height
#     ) |>
#     tibble::as_tibble() |>
#     dplyr::mutate(across(where(is.factor), as.character))

#   out |>
#     dplyr::filter(temperature == "mild") |>
#     dplyr::pull(probability)

#   fhd_df_comp |>
#     dplyr::filter(temperature == "mild") |>
#     dplyr::pull(probability)

# })
