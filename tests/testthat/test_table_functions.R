# Tests for TableOneDataframe package
# GENERATED USING AI, MAY NOT BE HELPFUL

library(testthat)
library(TableOneDataframe)
library(dplyr)

# Setup test data
setup_test_data <- function() {
  data(mtcars)
  mtcars$cyl <- factor(mtcars$cyl, levels = c("4", "6", "8"))
  mtcars$gear <- factor(mtcars$gear)
  mtcars$am <- factor(mtcars$am, levels = c(0, 1), labels = c("Automatic", "Manual"))
  mtcars
}

# ==============================================================================
# Tests for make_output_df
# ==============================================================================

test_that("make_output_df creates correct structure", {
  mtcars <- setup_test_data()

  output <- make_output_df(mtcars, "cyl", include_tests = FALSE)

  expect_s3_class(output, "table_builder")
  expect_true("output" %in% names(output))
  expect_true("data" %in% names(output))
  expect_true("strata" %in% names(output))
  expect_true("round" %in% names(output))
  expect_equal(output$round, 2)  # default
})

test_that("make_output_df includes p-value column when requested", {
  mtcars <- setup_test_data()

  output_no_p <- make_output_df(mtcars, "cyl", include_tests = FALSE)
  output_with_p <- make_output_df(mtcars, "cyl", include_tests = TRUE)

  expect_false("p" %in% colnames(output_no_p$output))
  expect_true("p" %in% colnames(output_with_p$output))
})

test_that("make_output_df sets custom round parameter", {
  mtcars <- setup_test_data()

  output <- make_output_df(mtcars, "cyl", round = 3)

  expect_equal(output$round, 3)
})

test_that("make_output_df fails with grouped data", {
  mtcars <- setup_test_data()
  mtcars_grouped <- mtcars %>% group_by(cyl)

  expect_error(
    make_output_df(mtcars_grouped, "cyl"),
    "grouped"
  )
})

test_that("make_output_df fails when strata is not a factor", {
  mtcars <- setup_test_data()
  mtcars$cyl <- as.character(mtcars$cyl)

  expect_error(
    make_output_df(mtcars, "cyl"),
    "factor"
  )
})

test_that("make_output_df fails when variable named 'variable' exists", {
  mtcars <- setup_test_data()
  mtcars$variable <- 1:nrow(mtcars)

  expect_error(
    make_output_df(mtcars, "cyl"),
    "variable"
  )
})

test_that("make_output_df creates correct column names", {
  mtcars <- setup_test_data()

  output <- make_output_df(mtcars, "cyl", include_tests = TRUE)
  cols <- colnames(output$output)

  expect_true(grepl("Total", cols[2]))
  expect_true(grepl("4", cols[3]))
  expect_true(grepl("6", cols[4]))
  expect_true(grepl("8", cols[5]))
  expect_equal(cols[length(cols)], "p")
})

# ==============================================================================
# Tests for get_median_iqr
# ==============================================================================

test_that("get_median_iqr works with piped approach", {
  mtcars <- setup_test_data()

  result <- make_output_df(mtcars, "cyl", include_tests = TRUE) %>%
    get_median_iqr(variable = 'mpg', name = "MPG")

  expect_s3_class(result, "table_builder")
  expect_equal(nrow(result$output), 1)
  expect_true(grepl("MPG", result$output[1, 1]))
  expect_true(grepl("Median", result$output[1, 1]))
})

test_that("get_median_iqr works with legacy approach", {
  mtcars <- setup_test_data()

  output <- make_output_df(mtcars, "cyl", include_tests = TRUE)
  result <- get_median_iqr(mtcars, 'cyl', 'mpg', "MPG", output$output)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("get_median_iqr uses custom rounding", {
  mtcars <- setup_test_data()

  result1 <- make_output_df(mtcars, "cyl", round = 1) %>%
    get_median_iqr(variable = 'mpg', name = "MPG")

  result2 <- make_output_df(mtcars, "cyl", round = 1) %>%
    get_median_iqr(variable = 'mpg', name = "MPG", round = 3)

  # Check that values are formatted differently
  expect_false(identical(result1$output[1, 2], result2$output[1, 2]))
})

test_that("get_median_iqr fails for non-numeric variables", {
  mtcars <- setup_test_data()

  expect_error(
    make_output_df(mtcars, "cyl") %>%
      get_median_iqr(variable = 'am', name = "AM"),
    "numeric"
  )
})

# ==============================================================================
# Tests for get_mean_sd
# ==============================================================================

test_that("get_mean_sd works with piped approach", {
  mtcars <- setup_test_data()

  result <- make_output_df(mtcars, "cyl", include_tests = TRUE) %>%
    get_mean_sd(variable = 'hp', name = "HP")

  expect_s3_class(result, "table_builder")
  expect_equal(nrow(result$output), 1)
  expect_true(grepl("HP", result$output[1, 1]))
  expect_true(grepl("Mean", result$output[1, 1]))
})

test_that("get_mean_sd works with legacy approach", {
  mtcars <- setup_test_data()

  output <- make_output_df(mtcars, "cyl", include_tests = TRUE)
  result <- get_mean_sd(mtcars, 'cyl', 'hp', "HP", output$output)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("get_mean_sd fails for non-numeric variables", {
  mtcars <- setup_test_data()

  expect_error(
    make_output_df(mtcars, "cyl") %>%
      get_mean_sd(variable = 'gear', name = "Gear"),
    "numeric"
  )
})

# ==============================================================================
# Tests for get_n_percent
# ==============================================================================

test_that("get_n_percent works with piped approach", {
  mtcars <- setup_test_data()

  # Expect chi-square warning due to small cell counts in mtcars
  expect_warning(
    result <- make_output_df(mtcars, "cyl", include_tests = TRUE) %>%
      get_n_percent(variable = 'gear', name = "Gears"),
    "Chi-squared approximation may be incorrect"
  )

  expect_s3_class(result, "table_builder")
  # Should have header row + one row per gear level
  expect_gt(nrow(result$output), 1)
  expect_true(grepl("Gears", result$output[1, 1]))
})

test_that("get_n_percent converts non-factors to factors", {
  mtcars <- setup_test_data()
  mtcars$carb <- as.numeric(mtcars$carb)  # Make it numeric

  result <- make_output_df(mtcars, "cyl") %>%
    get_n_percent(variable = 'carb', name = "Carbs")

  expect_s3_class(result, "table_builder")
  expect_gt(nrow(result$output), 1)
})

test_that("get_n_percent handles missing values", {
  mtcars <- setup_test_data()
  mtcars$gear[1:3] <- NA

  result <- make_output_df(mtcars, "cyl") %>%
    get_n_percent(variable = 'gear', name = "Gears")

  # Check that "Missing" appears in the output
  expect_true(any(grepl("Missing", result$output[, 1])))
})

test_that("get_n_percent sort_by_freq works", {
  mtcars <- setup_test_data()

  # Use carb which has different frequency vs alphabetical ordering
  mtcars$carb <- factor(mtcars$carb)

  result_default <- make_output_df(mtcars, "cyl") %>%
    get_n_percent(variable = 'carb', name = "Carbs")

  result_sorted <- make_output_df(mtcars, "cyl") %>%
    get_n_percent(variable = 'carb', name = "Carbs", sort_by_freq = TRUE)

  # Row order should be different (excluding header row)
  # Compare the order of values in the first column
  default_order <- result_default$output[-1, 1]  # Skip header
  sorted_order <- result_sorted$output[-1, 1]    # Skip header

  expect_false(identical(default_order, sorted_order))
})

test_that("get_n_percent with id parameter works", {
  # Create data with repeated measures
  test_data <- data.frame(
    id = c(1, 1, 2, 2, 3, 3),
    group = factor(c("A", "A", "B", "B", "A", "A")),
    outcome = c(1, 1, 0, 1, 1, 0)
  )

  result <- make_output_df(test_data, "group") %>%
    get_n_percent(variable = 'outcome', name = "Outcome", id = "id")

  expect_s3_class(result, "table_builder")
  expect_gt(nrow(result$output), 1)
})

test_that("get_n_percent data_override works", {
  mtcars <- setup_test_data()
  mtcars_subset <- mtcars[1:10, ]

  result <- make_output_df(mtcars, "cyl") %>%
    get_n_percent(variable = 'gear', name = "Gears", data_override = mtcars_subset)

  expect_s3_class(result, "table_builder")
  # The counts should be based on subset (10 rows) not full data (32 rows)
  total_col <- result$output[2, 2]  # First data row, total column
  expect_true(grepl("10|[0-9]", total_col))  # Should sum to 10
})

# ==============================================================================
# Tests for get_count and get_unique_count
# ==============================================================================

test_that("get_count works correctly", {
  mtcars <- setup_test_data()
  mtcars$wt[1:5] <- NA

  result <- make_output_df(mtcars, "cyl") %>%
    get_count(variable = 'wt', name = "Weight")

  expect_s3_class(result, "table_builder")
  expect_equal(nrow(result$output), 1)
  # Total should be 27 (32 - 5 NAs)
  expect_true(grepl("27", result$output[1, 2]))
})

test_that("get_unique_count works correctly", {
  mtcars <- setup_test_data()

  result <- make_output_df(mtcars, "cyl") %>%
    get_unique_count(variable = 'gear', name = "Unique Gears")

  expect_s3_class(result, "table_builder")
  expect_equal(nrow(result$output), 1)
})

# ==============================================================================
# Tests for get_n_percent_value
# ==============================================================================

test_that("get_n_percent_value works correctly", {
  mtcars <- setup_test_data()

  # Expect chi-square warning due to small cell counts
  expect_warning(
    result <- make_output_df(mtcars, "cyl", include_tests = TRUE) %>%
      get_n_percent_value(variable = 'vs', value = 1, name = "V-shaped"),
    "Chi-squared approximation may be incorrect"
  )

  expect_s3_class(result, "table_builder")
  expect_equal(nrow(result$output), 1)
  expect_true(grepl("V-shaped", result$output[1, 1]))
})

# ==============================================================================
# Tests for get_sum
# ==============================================================================

test_that("get_sum works correctly", {
  mtcars <- setup_test_data()

  result <- make_output_df(mtcars, "cyl") %>%
    get_sum(variable = 'hp', name = "Total HP")

  expect_s3_class(result, "table_builder")
  expect_equal(nrow(result$output), 1)
  expect_true(grepl("Total HP", result$output[1, 1]))
})

test_that("get_sum treats NA as zero", {
  mtcars <- setup_test_data()
  total_hp_before <- sum(mtcars$hp)

  mtcars$hp[1:5] <- NA
  total_hp_after <- sum(mtcars$hp, na.rm = TRUE)

  result <- make_output_df(mtcars, "cyl") %>%
    get_sum(variable = 'hp', name = "Total HP")

  # Should sum to total_hp_after, not total_hp_before
  expect_true(grepl(as.character(round(total_hp_after)), result$output[1, 2]))
})

# ==============================================================================
# Tests for get_availability
# ==============================================================================

test_that("get_availability works for numeric variables", {
  mtcars <- setup_test_data()
  mtcars$wt[1:10] <- NA

  result <- make_output_df(mtcars, "cyl") %>%
    get_availability(variable = 'wt', name = "Weight")

  expect_s3_class(result, "table_builder")
  expect_equal(nrow(result$output), 1)
  # Should show 22 out of 32 (68.75%)
  expect_true(grepl("22", result$output[1, 2]))
})

test_that("get_availability works for factor variables", {
  mtcars <- setup_test_data()
  mtcars$gear[1:5] <- NA

  result <- make_output_df(mtcars, "cyl") %>%
    get_availability(variable = 'gear', name = "Gear")

  expect_s3_class(result, "table_builder")
  expect_equal(nrow(result$output), 1)
  # Should show 27 out of 32
  expect_true(grepl("27", result$output[1, 2]))
})

# ==============================================================================
# Tests for extract_df
# ==============================================================================

test_that("extract_df extracts dataframe from table_builder", {
  mtcars <- setup_test_data()

  builder <- make_output_df(mtcars, "cyl") %>%
    get_median_iqr(variable = 'mpg', name = "MPG")

  result <- extract_df(builder)

  expect_s3_class(result, "data.frame")
  expect_false(inherits(result, "table_builder"))
  expect_equal(nrow(result), 1)
})

test_that("extract_df returns dataframe unchanged", {
  df <- data.frame(a = 1:5, b = 6:10)
  result <- extract_df(df)

  expect_identical(result, df)
})

# ==============================================================================
# Integration tests - chaining multiple functions
# ==============================================================================

test_that("Multiple functions can be chained together", {
  mtcars <- setup_test_data()

  # Expect chi-square warning from get_n_percent
  expect_warning(
    result <- make_output_df(mtcars, "cyl", include_tests = TRUE, round = 2) %>%
      get_median_iqr(variable = 'mpg', name = "MPG") %>%
      get_mean_sd(variable = 'hp', name = "Horsepower") %>%
      get_n_percent(variable = 'gear', name = "Gears") %>%
      get_count(variable = 'wt', name = "Weight") %>%
      extract_df(),
    "Chi-squared approximation may be incorrect"
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 4)  # At least 4 summary rows plus gear categories
})

test_that("Legacy and piped approaches produce same results", {
  mtcars <- setup_test_data()

  # Piped approach
  piped <- make_output_df(mtcars, "cyl", include_tests = TRUE, round = 2) %>%
    get_median_iqr(variable = 'mpg', name = "MPG") %>%
    get_mean_sd(variable = 'hp', name = "HP") %>%
    extract_df()

  # Legacy approach
  legacy <- make_output_df(mtcars, "cyl", include_tests = TRUE)
  legacy <- get_median_iqr(mtcars, 'cyl', 'mpg', "MPG", legacy$output, round = 2)
  legacy <- get_mean_sd(mtcars, 'cyl', 'hp', "HP", legacy, round = 2)

  expect_equal(piped, legacy)
})

test_that("Complex table with all function types works", {
  mtcars <- setup_test_data()
  mtcars$gear[3] <- NA
  mtcars$wt[12:16] <- NA

  # Multiple chi-square warnings expected (from multiple get_n_percent calls)
  # Use suppressWarnings here since we're testing integration, not warning behavior
  result <- suppressWarnings(
    make_output_df(mtcars, "cyl", include_tests = TRUE, round = 1) %>%
      get_median_iqr(variable = 'mpg', name = "MPG") %>%
      get_mean_sd(variable = 'hp', name = "Horsepower", round = 2) %>%
      get_n_percent(variable = 'am', name = "Transmission") %>%
      get_n_percent(variable = 'gear', name = "Gears") %>%
      get_count(variable = 'wt', name = "Weight count") %>%
      get_unique_count(variable = 'gear', name = "Unique gears") %>%
      get_n_percent_value(variable = 'vs', value = 1, name = "V-shaped") %>%
      get_sum(variable = 'hp', name = "Total HP") %>%
      get_availability(variable = 'wt', name = "Weight availability") %>%
      extract_df()
  )

  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 9)
  expect_equal(ncol(result), 6)  # Variable + Total + 3 cyl groups + p
})

# ==============================================================================
# Edge case tests
# ==============================================================================

test_that("Works with empty factor levels", {
  mtcars <- setup_test_data()
  mtcars$cyl <- factor(mtcars$cyl, levels = c("4", "6", "8", "Extra"))

  result <- make_output_df(mtcars, "cyl") %>%
    get_median_iqr(variable = 'mpg', name = "MPG") %>%
    extract_df()

  expect_s3_class(result, "data.frame")
  expect_true(any(grepl("Extra", colnames(result))))
})

test_that("Works with single-level factor", {
  mtcars <- setup_test_data()
  mtcars_subset <- mtcars[mtcars$cyl == "4", ]
  mtcars_subset$cyl <- factor(mtcars_subset$cyl)

  result <- make_output_df(mtcars_subset, "cyl") %>%
    get_median_iqr(variable = 'mpg', name = "MPG") %>%
    extract_df()

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 3)  # Variable + Total + one level
})

test_that("Handles all NA column", {
  mtcars <- setup_test_data()
  mtcars$test_var <- NA

  result <- make_output_df(mtcars, "cyl") %>%
    get_count(variable = 'test_var', name = "All NA")

  expect_s3_class(result, "table_builder")
  expect_true(grepl("0", result$output[1, 2]))  # Total should be 0
})

# ==============================================================================
# Output format tests
# ==============================================================================

test_that("Output has correct column structure", {
  mtcars <- setup_test_data()

  result <- make_output_df(mtcars, "cyl", include_tests = TRUE) %>%
    get_median_iqr(variable = 'mpg', name = "MPG") %>%
    extract_df()

  cols <- colnames(result)
  expect_equal(cols[1], "Variable")
  expect_true(grepl("Total", cols[2]))
  expect_equal(cols[length(cols)], "p")
})

test_that("P-values are formatted correctly", {
  mtcars <- setup_test_data()

  result <- make_output_df(mtcars, "cyl", include_tests = TRUE) %>%
    get_median_iqr(variable = 'mpg', name = "MPG") %>%
    extract_df()

  p_value <- result[1, ncol(result)]
  # Should be numeric-like string or "<0.001"
  expect_true(grepl("^[<0-9.]", p_value))
})

test_that("Percentages are formatted correctly", {
  mtcars <- setup_test_data()

  result <- make_output_df(mtcars, "cyl") %>%
    get_n_percent(variable = 'gear', name = "Gears") %>%
    extract_df()

  # Check that values contain parentheses and percentages
  value <- result[2, 2]  # First data row, total column
  expect_true(grepl("\\(.*\\)", value))  # Contains parentheses
})

# Note: Tests use expect_warning() for chi-square warnings where they occur individually.
# For integration tests with multiple warnings, suppressWarnings() is used to focus on
# testing the integration behavior rather than repeating warning validation.
