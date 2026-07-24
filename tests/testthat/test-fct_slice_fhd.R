# Shared fixture: 3 heights x 5 draws x 6 covariate combinations (wind x temp)
make_fhd_arr <- function() {
  fhd_df_to_array(
    expand.grid(
      draw_id = 1:5,
      height = seq(0.5, 2.5, by = 1),
      wind_speed = c("high", "low"),
      temperature = c("hot", "mild", "low")
    ) |>
      dplyr::mutate(probability = runif(dplyr::n())),
    height_col = "height",
    draw_col = "draw_id",
    prob_col = "probability"
  )
}


# 2D input (no covariates) ----------------------------------------------------

test_that("slice_fhd() passes through a covariate-free 2D array unchanged (early exit)", {
  arr_2d <- fhd_df_to_array(
    expand.grid(draw_id = 1:5, height = seq(0.5, 2.5, by = 1)) |>
      dplyr::mutate(probability = runif(dplyr::n())),
    height_col = "height",
    draw_col = "draw_id",
    prob_col = "probability"
  )

  out <- slice_fhd(arr_2d, out_format = "array")
  expect_equal(dim(out), dim(arr_2d))
  expect_equal(out, arr_2d)
  expect_false(attr(out, "fhd")$resampled %||% FALSE)
})


# Direct slice (no covariates dropped) ----------------------------------------

test_that("slice_fhd() slices to a single covariate level correctly", {
  arr <- make_fhd_arr()
  out <- slice_fhd(
    arr,
    wind_speed = "high",
    temperature = "hot",
    out_format = "array"
  )

  expect_equal(dim(out), c(3L, 5L, 1L), ignore_attr = TRUE)
  expect_equal(dimnames(out)$covs, "high_hot")
  # values must match the corresponding slice of the original array
  expect_equal(out[,, 1L], arr[,, "high_hot"])
  expect_false(attr(out, "fhd")$resampled)
  expect_null(attr(out, "fhd")$covs_dropped)
})

test_that("slice_fhd() slices to multiple levels across both covariates", {
  arr <- make_fhd_arr()
  out <- slice_fhd(
    arr,
    wind_speed = c("high", "low"),
    temperature = c("hot", "mild"),
    out_format = "array"
  )

  expect_equal(dim(out)[[3L]], 4L) # 2 wind x 2 temp combos
  expect_setequal(
    dimnames(out)$covs,
    c("high_hot", "high_mild", "low_hot", "low_mild")
  )
  expect_false(attr(out, "fhd")$resampled)
})


# Resampling (covariates dropped) ---------------------------------------------

test_that("slice_fhd() resamples when one covariate is dropped", {
  arr <- make_fhd_arr()
  out <- slice_fhd(
    arr,
    wind_speed = "high",
    temperature = NULL,
    seed = 42,
    out_format = "array"
  )

  # one retained covariate level -> one output slice
  expect_equal(dim(out), c(3L, 5L, 1L), ignore_attr = TRUE)
  expect_equal(dimnames(out)$covs, "high")
  expect_false(anyNA(out))
  expect_true(attr(out, "fhd")$resampled)
  expect_equal(attr(out, "fhd")$covs_dropped, "temperature")
})

test_that("slice_fhd() produces a 2D array when all covariates are dropped", {
  arr <- make_fhd_arr()
  out <- slice_fhd(
    arr,
    wind_speed = NULL,
    temperature = NULL,
    seed = 99,
    out_format = "array"
  )

  expect_equal(length(dim(out)), 2L)
  expect_equal(dim(out)[[1L]], dim(arr)[[1L]]) # heights unchanged
  expect_equal(dim(out)[[2L]], dim(arr)[[2L]]) # same ndraws
  expect_false(anyNA(out))
  expect_true(attr(out, "fhd")$resampled)
  expect_setequal(attr(out, "fhd")$covs_dropped, c("wind_speed", "temperature"))
})
