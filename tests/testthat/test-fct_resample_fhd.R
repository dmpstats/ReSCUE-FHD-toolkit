# Small, hand-verifiable fixture: 2 heights x 2 draws x 2 wind_speed levels,
# forming a complete grid with known probability values.
make_fhd_df <- function() {
  tibble::tribble(
    ~height , ~draw_id , ~wind_speed , ~probability ,
          5 ,        1 , "high"      , 0.10         ,
         10 ,        1 , "high"      , 0.20         ,
          5 ,        2 , "high"      , 0.30         ,
         10 ,        2 , "high"      , 0.40         ,
          5 ,        1 , "low"       , 0.50         ,
         10 ,        1 , "low"       , 0.60         ,
          5 ,        2 , "low"       , 0.70         ,
         10 ,        2 , "low"       , 0.80
  )
}

# resample_fhd() ------------------------------------------------------------------------

test_that("testing resample_fhd()", {
  skip("dev testing")

  fhd_df <- expand.grid(
    draw_id = 1:5,
    height = seq(0.5, 2.5, by = 1),
    wind_speed = c("high", "low"),
    temperature = c("hot", "mild", "low"),
    daytime = c(TRUE, FALSE)
  ) |>
    dplyr::mutate(probability = runif(length(draw_id)))

  fhd_arr <- fhd_df_to_array(
    fhd_df,
    height_col = "height",
    draw_col = "draw_id",
    prob_col = "probability"
  )

  slice_fhd(
    fhd_array = fhd_arr,
    wind_speed = c("high"),
    temperature = c("hot", "mild"),
    daytime = NULL, #c(TRUE),
    n_resamp = 5,
    out_format = "df"
  )

  slice_fhd(
    fhd_array = fhd_arr,
    wind_speed = NULL, # c("high"),
    temperature = NULL,
    n_resamp = 5
  )
})


# fhd_df_to_array() ---------------------------------------------------------------------

test_that("fhd_df_to_array() produces correct dimensions and dimnames", {
  arr <- fhd_df_to_array(make_fhd_df())

  expect_equal(
    dim(arr),
    c(n_height = 2L, n_draws = 2L, n_covs_combinations = 2L)
  )
  expect_named(dimnames(arr), c("height", "draw_id", "covs"))
  expect_equal(dimnames(arr)$height, c("5", "10"))
  expect_equal(dimnames(arr)$draw_id, c("1", "2"))
  expect_equal(dimnames(arr)$covs, c("high", "low"))
})

test_that("fhd_df_to_array() places values in the correct cells", {
  arr <- fhd_df_to_array(make_fhd_df())

  # column-major within each slice: (height5,draw1), (height10,draw1), (height5,draw2), (height10,draw2)
  expect_equal(as.numeric(arr[,, "high"]), c(0.10, 0.20, 0.30, 0.40))
  expect_equal(as.numeric(arr[,, "low"]), c(0.50, 0.60, 0.70, 0.80))
})

test_that("fhd_df_to_array() handles multiple covariate columns", {
  df <- make_fhd_df() |>
    dplyr::cross_join(tibble::tibble(month = c("May", "Jun")))

  arr <- fhd_df_to_array(df)

  expect_equal(dim(arr)[["n_covs_combinations"]], 4L)
  expect_equal(
    sort(dimnames(arr)$covs),
    sort(c("high_Jun", "high_May", "low_Jun", "low_May"))
  )
})


test_that("fhd_df_to_array() warns and fills NA for missing combinations", {
  df <- make_fhd_df()[-8, ] # drop (height=10, draw=2, wind_speed="low")

  expect_warning(
    arr <- fhd_df_to_array(df),
    "missing some combinations"
  )

  expect_equal(sum(is.na(arr)), 1L)
  expect_true(is.na(arr["10", "2", "low"]))
  # unaffected cells stay intact
  expect_equal(arr["5", "1", "high"], 0.10)
})

test_that("fhd_df_to_array() attaches cov_combos and col_names attributes", {
  arr <- fhd_df_to_array(make_fhd_df())

  expect_equal(
    attr(arr, "cov_combos"),
    tibble::tibble(wind_speed = c("high", "low"))
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


# fhd_array_to_df() ---------------------------------------------------------------------
test_that("fhd_array_to_df() round-trips a single-covariate array exactly", {
  df <- make_fhd_df()
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
  df <- make_fhd_df() |>
    dplyr::cross_join(tibble::tibble(month = c("May", "Jun")))
  arr <- fhd_df_to_array(df)
  df_back <- fhd_array_to_df(arr)

  df_sorted <- dplyr::arrange(df, wind_speed, month, draw_id, height)
  df_back_sorted <- dplyr::arrange(df_back, wind_speed, month, draw_id, height)

  expect_equal(df_back_sorted$probability, df_sorted$probability)
  expect_equal(df_back_sorted$month, df_sorted$month)
  expect_equal(df_back_sorted$wind_speed, df_sorted$wind_speed)
})

test_that("fhd_array_to_df() respects column name overrides", {
  arr <- fhd_df_to_array(make_fhd_df())
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
    "cov_combos"
  )
})

test_that("fhd_array_to_df() errors when array is not 3-dimensional", {
  arr <- fhd_df_to_array(make_fhd_df())
  matrix_arr <- arr[,, 1] # drop to 2D, keeping cov_combos/col_names attributes
  attr(matrix_arr, "cov_combos") <- attr(arr, "cov_combos")
  attr(matrix_arr, "col_names") <- attr(arr, "col_names")

  expect_error(
    fhd_array_to_df(matrix_arr),
    "3D array"
  )
})
