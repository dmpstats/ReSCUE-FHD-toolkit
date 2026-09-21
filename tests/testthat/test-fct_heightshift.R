# Functions for small, hand-verifiable datasets,w ith probability mass at 1m height bands from 0 - 5m. `prob` sums to 1 for each FHD/draw combination.

## single FHD/draw
mock_hs_df <- function() {
  dplyr::tibble(
    fhd_id = "A",
    height = seq(0.5, 10.5, 1),
    prob = c(0.05, 0.05, 0.025, 0.2, 0.25, 0.15, 0.1, 0.05, 0.05, 0.05, 0.025),
    draw_id = 1
  )
}

# two FHDs, different height extents, single draw.
mock_hs_df_multi_fhd <- function() {
  rbind(
    mock_hs_df(),
    data.frame(
      fhd_id = "B",
      height = seq(0.5, 7.5, 1),
      prob = c(0.05, 0.075, 0.1, 0.15, 0.2, 0.25, 0.1, 0.075),
      draw_id = 1
    )
  )
}

# Two draws for the same FHD, differing only at the baseline window heights.
mock_hs_df_multi_draw <- function() {
  data.frame(
    fhd_id = "A",
    height = rep(seq(0.5, 5.5, 1), 2),
    prob = c(0.2, 0.3, 0.2, 0.15, 0.1, 0.05, 0.05, 0.05, 0.1, 0.2, 0.3, 0.3),
    draw_id = rep(1:2, each = 6)
  )
}


# Output structure  -------------------------------------------------------------

test_that("heightshift() returns a list of matrices and optional tibble", {
  # default structure
  out <- heightshift(
    mock_hs_df(),
    risk_min = 4,
    risk_max = 6,
    max_shift = 3,
    step_size = 1
  )

  expect_type(out, "list")
  expect_named(out, c("prob", "perc"))
  expect_true(is.matrix(out$prob))
  expect_true(is.matrix(out$perc))

  # first column named "fhd_id", remaining columns are height offsets
  expect_equal(unname(out$prob[, "fhd_id"]), "A")
  expect_equal(colnames(out$prob)[1], "fhd_id")

  # Baseline column (+0m) is flagged via the true_fhd_col attribute
  bsln_col <- attr(out$prob, "bsl_crh_idx")
  expect_equal(colnames(out$prob)[bsln_col], "+0m")
  expect_equal(attr(out$perc, "bsl_crh_idx"), bsln_col)

  # expected number of columns, given risk-window, max_shift and step
  # max_shift * 2 (up and down) + 1 (baseline) + 1 (fhd_id) = 8
  expect_equal(dim(out$prob), c(1L, 8L))
  expect_equal(dim(out$perc), c(1L, 8L))

  # with shift table, expect a third element in the list, which is a tibble
  out <- heightshift(
    mock_hs_df(),
    risk_min = 4,
    risk_max = 6,
    max_shift = 3,
    step_size = 1,
    crh_windows = TRUE
  )

  # tibble has same nrows as ncols in matrices
  expect_equal(nrow(out$heightshifts), ncol(out$prob) - 1) # deduct fhd_id column
  # output is a list with three elements, the third being a tibble
  expect_named(out, c("prob", "perc", "heightshifts"))

  expect_true(tibble::is_tibble(out$heightshifts))

  expect_equal(
    which(out$heightshifts$shift == 0),
    attr(out$prob, "bsl_crh_idx") - 1 # deduct fhd_id column
  )
})


test_that("Output dimensions under max_shift and step_size as expected", {
  ##  max_shift > airgap, step_size = 1 ----------------------------
  out <- heightshift(
    mock_hs_df(),
    risk_min = 4,
    risk_max = 6,
    max_shift = 10,
    step_size = 1,
    crh_windows = TRUE
  )

  # only 4 negative shifts possible, given risk_min = 4 and step_size = 1
  expect_equal(sum(out$heightshifts$shift < 0), 4)
  # 10 positive shifts expected
  expect_equal(sum(out$heightshifts$shift > 0), 10)

  # expected shifts
  expect_equal(
    out$heightshifts$shift,
    seq(-4, 10, by = 1)
  )

  # outputs consistent in number of CRH windows
  expect_equal(
    nrow(out$heightshifts),
    ncol(out$prob) - 1 # deduct fhd_id column
  )
  expect_equal(ncol(out$prob), ncol(out$perc))

  # shifts consistent
  expect_equal(
    out$heightshifts$shift,
    as.numeric(gsub("m", "", colnames(out$prob)[-1]))
  )

  ##  step_size = 5 --------------------------
  out <- heightshift(
    mock_hs_df(),
    risk_min = 4,
    risk_max = 6,
    max_shift = 10,
    step_size = 5,
    crh_windows = TRUE
  )

  # no negative shifts possible, as step_size > risk_min
  expect_equal(sum(out$heightshifts$shift < 0), 0)
  # two positive shifts expected, +0m and +5m
  expect_equal(sum(out$heightshifts$shift > 0), 2)

  expect_equal(
    nrow(out$heightshifts),
    ncol(out$prob) - 1 # deduct fhd_id column
  )
  expect_equal(ncol(out$prob), ncol(out$perc))

  ## two FHDs --------------------------------
  out <- heightshift(
    mock_hs_df_multi_fhd(),
    risk_min = 4,
    risk_max = 6,
    max_shift = 10,
    step_size = 1,
    crh_windows = TRUE
  )

  # only 4 negative shifts possible, given risk_min = 4 and step_size = 1
  expect_equal(sum(out$heightshifts$shift < 0), 4)
  # 10 positive shifts expected
  expect_equal(sum(out$heightshifts$shift > 0), 10)

  # expected shifts
  expect_equal(
    out$heightshifts$shift,
    seq(-4, 10, by = 1)
  )

  # outputs consistent in number of CRH windows
  expect_equal(
    nrow(out$heightshifts),
    ncol(out$prob) - 1 # deduct fhd_id column
  )
  expect_equal(ncol(out$prob), ncol(out$perc))

  # 2 rows in matrices, one for each FHD
  expect_true(nrow(out$prob) == 2)
  expect_true(nrow(out$perc) == 2)

  ##  step_size == airgap --------------------------
  out <- heightshift(
    mock_hs_df(),
    risk_min = 5,
    risk_max = 7,
    max_shift = 20,
    step_size = 5,
    crh_windows = TRUE
  )

  # only 1 negative shift possible, given risk_min = 5 and step_size = 5
  expect_equal(sum(out$heightshifts$shift < 0), 1)
  # 4 positive shifts expected, +0m, +5m, +10m, +20m
  expect_equal(sum(out$heightshifts$shift > 0), 4)

  expect_equal(
    out$heightshifts$shift,
    c(-5, 0, 5, 10, 15, 20)
  )

  # outputs consistent in number of CRH windows
  expect_equal(
    nrow(out$heightshifts),
    ncol(out$prob) - 1 # deduct fhd_id column
  )
  expect_equal(ncol(out$prob), ncol(out$perc))
})


# Output validation  -------------------------------------------------------------

test_that("heightshift() computes probability values matching hand-calculated sums", {
  # steo_size = 1
  out <- heightshift(
    mock_hs_df(),
    risk_min = 2,
    risk_max = 4,
    max_shift = 20
  )
  expected <- c(
    "-2m" = 0.1,
    "-1m" = 0.075,
    "+0m" = 0.225,
    "+1m" = 0.45,
    "+2m" = 0.4,
    "+3m" = 0.25,
    "+4m" = 0.15,
    "+5m" = 0.1,
    "+6m" = 0.1,
    "+7m" = 0.075,
    "+8m" = 0.025,
    "+9m" = 0,
    "+10m" = 0,
    "+20m" = 0
  )

  actual <- as.numeric(out$prob[1, names(expected)])
  names(actual) <- names(expected)
  expect_equal(actual, expected)

  # step_size == airgap
  out <- heightshift(
    mock_hs_df(),
    risk_min = 5,
    risk_max = 8,
    max_shift = 30,
    step_size = 5,
    crh_windows = TRUE
  )

  expected <- c(
    "-5m" = 0.125,
    "+0m" = 0.30,
    "+5m" = 0.025,
    "+10m" = 0,
    "+15m" = 0,
    "+20m" = 0,
    "+25m" = 0,
    "+30m" = 0
  )

  actual <- as.numeric(out$prob[1, names(expected)])
  names(actual) <- names(expected)
  expect_equal(actual, expected)
})


test_that("heightshift() computes percentage changes relative to the baseline", {
  # step_size = 1
  out <- heightshift(
    mock_hs_df(),
    risk_min = 5,
    risk_max = 7,
    max_shift = 10,
    step_size = 1,
    crh_windows = TRUE
  )

  baseline <- 0.25
  expect_equal(as.numeric(out$perc[1, "+0m"]), 0)
  expect_equal(
    as.numeric(out$perc[1, "-5m"]),
    (0.1 - baseline) / baseline * 100
  )

  expect_equal(
    as.numeric(out$perc[1, "-2m"]),
    (0.45 - baseline) / baseline * 100
  )

  expect_equal(
    as.numeric(out$perc[1, "+2m"]),
    (0.1 - baseline) / baseline * 100
  )

  expect_equal(
    as.numeric(out$perc[1, "+5m"]),
    (0.025 - baseline) / baseline * 100
  )

  expect_equal(
    as.numeric(out$perc[1, "+10m"]),
    (0 - baseline) / baseline * 100
  )

  # step_size = 5
  out <- heightshift(
    mock_hs_df(),
    risk_min = 5,
    risk_max = 7,
    max_shift = 20,
    step_size = 5,
    crh_windows = TRUE
  )

  baseline <- 0.25
  expect_equal(as.numeric(out$perc[1, "+0m"]), 0)
  expect_equal(
    as.numeric(out$perc[1, "-5m"]),
    (0.1 - baseline) / baseline * 100
  )

  expect_equal(
    as.numeric(out$perc[1, "+5m"]),
    (0.025 - baseline) / baseline * 100
  )
})


# Multiple FHDs -----------------------------------------------------------

test_that("heightshift() computes each FHD independently", {
  out <- heightshift(
    mock_hs_df_multi_fhd(),
    risk_min = 2,
    risk_max = 4,
    max_shift = 10
  )

  expect_equal(rownames(out$prob), c("A", "B"))

  expect_equal(as.numeric(out$prob["A", "+0m"]), 0.225)
  expect_equal(as.numeric(out$prob["A", "+4m"]), 0.15)
  expect_equal(as.numeric(out$prob["A", "+8m"]), 0.025)

  expect_equal(as.numeric(out$prob["B", "+0m"]), 0.25)
  expect_equal(as.numeric(out$prob["B", "-2m"]), 0.125)
  expect_equal(as.numeric(out$prob["B", "+8m"]), 0.0)
})


# Multiple draws (averaging) ---------------------------------------------------

test_that("heightshift() averages probabilities across draws", {
  out <- heightshift(
    mock_hs_df_multi_draw(),
    risk_min = 2,
    risk_max = 4,
    max_shift = 10,
    step_size = 1
  )

  # draw 1 baseline window sum = 0.35, draw 2 baseline window sum = 0.30
  expect_equal(as.numeric(out$prob[1, "+0m"]), mean(c(0.35, 0.30)))

  # draw 1 +3m window sum = 0.05, draw 2 +3m window sum = 0.30
  expect_equal(as.numeric(out$prob[1, "+3m"]), mean(c(0.05, 0.30)))
})


# Edge cases ----------------------------------------------------------------

test_that("heightshift() handles CRH wider than the whole FHD extent", {
  # CRH 0 - 100m >>> FHD extent (0 - 10m)
  # step_size = 1
  out <- heightshift(
    mock_hs_df(), # heights span 0.5 - 10.5
    risk_min = 0,
    risk_max = 100,
    max_shift = 20,
    step_size = 1
  )

  # the first CRH window contains the whole probability mass
  expect_equal(as.numeric(out$prob[1, "+0m"]), 1)

  # the 2nd CRH window contains 1 - 0.025 (pm at first height-class)
  expect_equal(as.numeric(out$prob[1, "+1m"]), 1 - 0.05)

  # the +10m CRH window contains only the probability mass at the final height-class
  expect_equal(as.numeric(out$prob[1, "+10m"]), 0.025)

  # from the +11m onwards, probability mass is 0, as the CRH window is above the FHD's extent
  expect_equal(as.numeric(out$prob[1, "+11m"]), 0)
  expect_equal(as.numeric(out$prob[1, "+15m"]), 0)
  expect_equal(as.numeric(out$prob[1, "+20m"]), 0)

  # all %-changes from +1m are negative as window moves gradually outside the FHD extend
  expect_true(all(as.numeric(out$perc[1, c(-1, -2)]) < 0))
})


test_that("heightshift() handles baseline CRH outside FHD's extent", {
  ## single FHD ------
  out <- heightshift(
    mock_hs_df(), # heights span 0.5 - 10.5
    risk_min = 11,
    risk_max = 14,
    max_shift = 10,
    step_size = 1,
    crh_windows = TRUE
  )

  # Baseline PCRH is zero, as the risk-window is above the FHD's extent
  expect_equal(as.numeric(out$prob[, "+0m"]), 0)

  # PCRHs at all positive shifts is zero
  expect_true(
    all(
      as.numeric(out$prob[, which(grepl("^\\+\\d+m$", colnames(out$prob)))]) ==
        0
    )
  )

  # PCRHs at all negative shifts are positive
  expect_true(
    all(
      as.numeric(out$prob[, which(grepl("^\\-\\d+m$", colnames(out$prob)))]) > 0
    )
  )

  # check against hand-calculated values for negative shifts
  expect_equal(as.numeric(out$prob[, "-1m"]), 0.025)
  expect_equal(as.numeric(out$prob[, "-5m"]), 0.20)

  # %-change at all negative shifts is Inf, as the baseline PCRH is zero
  expect_true(
    all(
      is.infinite(as.numeric(out$perc[, which(grepl(
        "^\\-\\d+m$",
        colnames(out$perc)
      ))]))
    )
  )

  # %-change at all positive shifts is NA, as the baseline PCRH is zero
  expect_true(
    all(
      is.na(as.numeric(out$perc[, which(grepl(
        "^\\+\\d+m$",
        colnames(out$perc)
      ))]))
    )
  )

  # shifts span between -10m and +10m, given max_shift = 10 and step_size = 1
  expect_equal(
    as.numeric(gsub("m", "", colnames(out$prob)[-1])),
    seq(-10, 10, by = 1)
  )

  ## Multiple FHD  ---------
  out <- heightshift(
    mock_hs_df_multi_fhd(), # heights span 0.5 - 10.5 and 0.5 - 7.5
    risk_min = 11,
    risk_max = 14,
    max_shift = 10,
    step_size = 2,
    crh_windows = TRUE
  )

  # Baseline PCRH is zero, as the risk-window is above the FHD's extent
  expect_equal(as.numeric(out$prob[, "+0m"]), c(0, 0))

  # shifts span between -10m and +10m, given max_shift = 10 and step_size = 2
  expect_equal(
    as.numeric(gsub("m", "", colnames(out$prob)[-1])),
    seq(-10, 10, by = 2)
  )
})


test_that("heightshift() handles largest negative shift outside FHD's extent", {
  # single FHD
  out <- heightshift(
    mock_hs_df(), # heights span 0.5 - 10.5
    risk_min = 100,
    risk_max = 120,
    max_shift = 10,
    step_size = 5,
  )

  # PCRH at all shifts is zero, as the risk-window is above the FHD's extent
  expect_true(all(as.numeric(out$prob[, -1]) == 0))

  # %-change at all shifts is NA, as the PCRH at all shifts is zero
  expect_true(all(is.na(as.numeric(out$perc[, -1]))))
})


test_that("heightshift() handles an all-NA probability column", {
  df <- mock_hs_df()
  df$prob <- NA_real_

  out <- heightshift(df, risk_min = 5, risk_max = 10)

  # rowSums(..., na.rm = TRUE) treats the missing values as contributing 0
  expect_true(all(as.numeric(out$prob[1, -1]) == 0))
  # percentage change against a zero baseline is undefined
  expect_true(all(is.nan(as.numeric(out$perc[1, -1]))))
})
