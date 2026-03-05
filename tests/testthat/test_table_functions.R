# ==============================================================================
# Additional tests for argument handling changes
# Run these alongside the existing test file.
# ==============================================================================

library(testthat)
library(TableOneDataframe)
library(dplyr)

setup_test_data <- function() {
  data(mtcars)
  mtcars$cyl <- factor(mtcars$cyl, levels = c("4", "6", "8"))
  mtcars$gear <- factor(mtcars$gear)
  mtcars$am <- factor(mtcars$am, levels = c(0, 1), labels = c("Automatic", "Manual"))
  mtcars
}

# ==============================================================================
# Pipe mode: positional arg guard
# Passing variable positionally (without naming it) in pipe mode should give
# a clear, actionable error — not a cryptic downstream failure.
# ==============================================================================

test_that("pipe mode gives clear error when strata passed positionally", {
  mtcars <- setup_test_data()
  builder <- make_output_df(mtcars, "cyl")

  expect_error(
    get_median_iqr(builder, 'mpg', "MPG"),
    "pipe mode"
  )
})

test_that("pipe mode error message names the offending argument", {
  mtcars <- setup_test_data()
  builder <- make_output_df(mtcars, "cyl")

  expect_error(
    get_count(builder, 'wt', "Weight"),
    "wt"
  )
})

test_that("pipe mode error fires for all get_* functions", {
  mtcars <- setup_test_data()
  builder <- make_output_df(mtcars, "cyl")

  expect_error(get_median_iqr(builder, 'mpg', "MPG"),        "pipe mode")
  expect_error(get_mean_sd(builder, 'hp', "HP"),             "pipe mode")
  expect_error(get_n_percent(builder, 'gear', "Gears"),      "pipe mode")
  expect_error(get_sum(builder, 'hp', "HP"),                 "pipe mode")
  expect_error(get_count(builder, 'wt', "Weight"),           "pipe mode")
  expect_error(get_unique_count(builder, 'gear', "Gears"),   "pipe mode")
  expect_error(get_availability(builder, 'wt', "Weight"),    "pipe mode")
})

test_that("pipe mode works correctly with named arguments", {
  mtcars <- setup_test_data()

  # These should all succeed — no error
  expect_no_error(
    make_output_df(mtcars, "cyl") %>%
      get_median_iqr(variable = 'mpg', name = "MPG") %>%
      get_mean_sd(variable = 'hp', name = "HP") %>%
      get_count(variable = 'wt', name = "Weight")
  )
})

# ==============================================================================
# Legacy mode: NULL guards
# Omitting strata or output in legacy mode should give a clear error,
# not a cryptic dplyr failure.
# ==============================================================================

test_that("legacy mode gives clear error when strata is NULL", {
  mtcars <- setup_test_data()
  output <- make_output_df(mtcars, "cyl")

  expect_error(
    get_median_iqr(mtcars, strata = NULL, variable = 'mpg', name = "MPG", output = output$output),
    "strata.*missing|missing.*strata"
  )
})

test_that("legacy mode gives clear error when output is NULL", {
  mtcars <- setup_test_data()

  expect_error(
    get_median_iqr(mtcars, strata = 'cyl', variable = 'mpg', name = "MPG", output = NULL),
    "output.*missing|missing.*output"
  )
})

test_that("legacy mode NULL guard fires for strata across all get_* functions", {
  mtcars <- setup_test_data()
  output <- make_output_df(mtcars, "cyl")$output

  expect_error(get_median_iqr(mtcars, NULL, 'mpg', "MPG", output),   "strata")
  expect_error(get_mean_sd(mtcars, NULL, 'hp', "HP", output),        "strata")
  expect_error(get_count(mtcars, NULL, 'wt', "Weight", output),      "strata")
  expect_error(get_sum(mtcars, NULL, 'hp', "HP", output),            "strata")
  expect_error(get_availability(mtcars, NULL, 'wt', "Weight", output), "strata")
})

# ==============================================================================
# round = NULL default: builder round value should be used
# ==============================================================================

test_that("round defaults to builder value when not specified in pipe mode", {
  mtcars <- setup_test_data()

  result_1dp <- make_output_df(mtcars, "cyl", round = 1) %>%
    get_mean_sd(variable = 'hp', name = "HP") %>%
    extract_df()

  result_3dp <- make_output_df(mtcars, "cyl", round = 3) %>%
    get_mean_sd(variable = 'hp', name = "HP") %>%
    extract_df()

  # Values should differ because rounding differs
  expect_false(identical(result_1dp[1, 2], result_3dp[1, 2]))
})

test_that("round override in get_* overrides builder default", {
  mtcars <- setup_test_data()

  result_default <- make_output_df(mtcars, "cyl", round = 1) %>%
    get_mean_sd(variable = 'hp', name = "HP") %>%
    extract_df()

  result_override <- make_output_df(mtcars, "cyl", round = 1) %>%
    get_mean_sd(variable = 'hp', name = "HP", round = 3) %>%
    extract_df()

  expect_false(identical(result_default[1, 2], result_override[1, 2]))
})

# ==============================================================================
# Signature defaults: strata = NULL and output = NULL should not cause
# RStudio / R CMD check warnings about missing arguments
# ==============================================================================

test_that("get_* functions accept missing strata and output without R-level error", {
  mtcars <- setup_test_data()
  builder <- make_output_df(mtcars, "cyl")

  # In pipe mode, strata and output are genuinely absent — this should work
  expect_no_error(
    builder %>% get_count(variable = 'wt', name = "Weight")
  )
})
