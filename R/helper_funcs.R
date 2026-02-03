#' Make output dataframe
#'
#' Includes neat column names with counts of observations in each group.
#' @param data tibble containing data. Can't be a grouped tibble. Also, can't have a variable named 'variable'.
#' @param strata A factor variable to stratify by.
#' @param include_tests Should it create a space for p-values?
#' @param round Default number of decimal places for all subsequent operations
#' @import dplyr
#' @export
make_output_df <- function(data, strata, include_tests = FALSE, round = 2) {
  # Data can't be grouped already
  if (is_grouped_df(data)) {
    stop("The `data` provided is grouped. This is cause issues with later functions.
       Fix this by running `data <- ungroup(data)`")
  }
  # Variable needs to be a factor.
  if (!is.factor(data[[strata]])) {
    stop("Strata variable needs to be a factor")
  }
  # Can't allow variables with the same name as the argument of subsequent functions.
  if ("variable" %in% colnames(data) | "Variable" %in% colnames(data)) {
    stop("There is a variable in the dataframe named 'variable'. Please rename it and try again.")
  }
  # Getting levels counts
  levels <- levels(data[[strata]])
  strata_names <- data %>%
    group_by(get(strata), .drop = FALSE) %>%
    summarise(n = n()) %>%
    mutate(names = paste0(`get(strata)`, " (N=", n, ")"))  # FIX: Added backticks
  total_name <- paste0("Total", " (N=", nrow(data), ")")
  if (include_tests) {
    output <- data.frame(matrix(ncol = 3 + length(levels), nrow = 0))
    colnames(output) <- c("Variable", total_name, strata_names$names, "p")
  } else {
    output <- data.frame(matrix(ncol = 2 + length(levels), nrow = 0))
    colnames(output) <- c("Variable", total_name, strata_names$names)
  }
  # Return a table_builder object
  structure(
    list(
      output = output,
      data = data,
      strata = strata,
      round = round
    ),
    class = "table_builder"
  )
}

#' Extract parameters from table_builder or use direct arguments
#' @keywords internal
extract_params <- function(data_or_builder, strata, output, round, data_override = NULL) {
  if (inherits(data_or_builder, "table_builder")) {
    # Builder mode: extract from object, but allow overrides
    list(
      data = if (!is.null(data_override)) data_override else data_or_builder$data,
      strata = if (!missing(strata) && !is.null(strata)) strata else data_or_builder$strata,
      output = if (!missing(output) && !is.null(output)) output else data_or_builder$output,
      round = if (!missing(round) && !is.null(round)) round else data_or_builder$round,
      builder_mode = TRUE,
      builder = data_or_builder
    )
  } else {
    # Legacy mode: data_or_builder is the actual data
    # BUT output might be a table_builder object from a previous call
    actual_output <- if (inherits(output, "table_builder")) output$output else output

    list(
      data = data_or_builder,
      strata = strata,
      output = actual_output,
      round = if (!missing(round)) round else 2,
      builder_mode = FALSE,
      builder = NULL
    )
  }
}

#' Return output in appropriate format
#' @keywords internal
return_output <- function(params, new_output) {
  if (params$builder_mode) {
    # Update and return builder
    params$builder$output <- new_output
    return(params$builder)
  } else {
    # Return just the dataframe
    return(new_output)
  }
}

format_num <- function(x, dp) {
  # 1. Round the number 'x' to the desired number of decimal places 'dp'.
  x_rounded <- round(x, dp)

  # 2. Format the rounded number as a character string.
  #    'nsmall = dp' forces the display of 'dp' number of zeros,
  #    e.g., 5 becomes "5.00" if dp=2.
  #    'trim = TRUE' removes any leading/trailing white space added by format().
  formatted_x <- format(x_rounded, nsmall = dp, trim = TRUE)

  return(formatted_x)
}


format_pvalue <- function(p_value) {
  # 1. Check for the < 0.001 threshold
  if (p_value < 0.001) {
    return("<0.001")
  } else {
    # 2. Use the format logic for padding (always 3 dps for p-values)
    # Note: We hardcode '3' instead of using the 'round' argument here
    # because p-values are conventionally reported to 3 dps.
    return(format(round(p_value, 3), nsmall = 3, trim = TRUE))
  }
}

#' Extract dataframe from table_builder object
#'
#' Convenience function to extract the final dataframe from a table_builder object.
#' If passed a regular dataframe, returns it unchanged (for backward compatibility).
#'
#' @param builder A table_builder object from make_output_df() or a regular dataframe
#' @return A dataframe containing the table
#' @export
#' @examples
#' \dontrun{
#' table <- make_output_df(mtcars, "cyl") %>%
#'   get_median_iqr(variable = 'mpg', name = "MPG") %>%
#'   extract_df()
#' }
extract_df <- function(builder) {
  if (inherits(builder, "table_builder")) {
    return(builder$output)
  }
  builder
}
