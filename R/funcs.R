#' Get Median and IQR for a particular variable.
#'
#' Does a Mann-Whitney/Kruskal-Wallis test if the output data frame has a p value column.
#'
#' @param data A tibble (legacy mode) or a table_builder object from \code{make_output_df()} (pipe mode).
#' @param strata The factor variable to stratify by. Must be a factor (character and numeric are not accepted).
#'   \strong{In pipe mode, omit this argument} — it is read from the builder object.
#'   Passing it positionally in pipe mode will throw a clear error; use named arguments if needed.
#'   In legacy mode, this argument is required.
#' @param variable Name of the numeric variable to summarise.
#' @param name String label for this row in the output table.
#' @param output The data frame to append results to.
#'   \strong{In pipe mode, omit this argument} — it is read from the builder object.
#'   In legacy mode, this argument is required.
#' @param round Number of decimal places. In pipe mode, omit to use the default set in \code{make_output_df()}. Can be overridden per-row if needed.
#'
#' @import dplyr
#'
#' @export
get_median_iqr <- function(data, strata = NULL, variable, name, output = NULL, round = NULL) {
  # Extract parameters (handles both modes)
  params <- extract_params(data, strata, output, round)

  if (!variable %in% names(params$data)) {
    stop("Variable '", variable, "' not found in data.")
  }
  # Data can't be grouped already
  if (is_grouped_df(params$data)) {
    stop("The `data` provided is grouped. This is cause issues with later functions.
       Fix this by running `data <- ungroup(data)`")
  }
  # Strata variable needs to be a factor.
  if (!is.factor(params$data[[params$strata]])) {
    stop("Strata variable needs to be a factor")
  }
  # Variable needs to be numeric
  if (!is.numeric(params$data[[variable]])) {
    stop("Variable needs to be numeric")
  }

  # Saving the column names for later.
  colnames <- colnames(params$output)

  by_strata <- params$data %>%
    group_by(.data[[params$strata]], .drop = FALSE) %>%
    summarise(clean = paste0(
      format_num(median(.data[[variable]], na.rm = TRUE), params$round), " (",
      format_num(quantile(.data[[variable]], 0.25, na.rm = TRUE), params$round), " - ",
      format_num(quantile(.data[[variable]], 0.75, na.rm = TRUE), params$round), ")"
    ), .groups = "drop") %>%
    select(clean) %>%
    t()

  # Now, getting the total to put at the beginning.
  total <- params$data %>%
    summarise(clean = paste0(
      format_num(median(.data[[variable]], na.rm = TRUE), params$round), " (",
      format_num(quantile(.data[[variable]], 0.25, na.rm = TRUE), params$round), " - ",
      format_num(quantile(.data[[variable]], 0.75, na.rm = TRUE), params$round), ")"
    )) %>%
    select(clean) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " Median (IQR)"), total, by_strata)

  ## Doing a Mann whitney/K-wallis test if there is a p value column.
  if ("p" %in% colnames) {
    test <- params$data %>%
      summarise(test = format_pvalue(kruskal.test(.data[[variable]] ~ .data[[params$strata]])$p.value)) %>%
      select(test) %>%
      t()
    all <- c(all, test)
  }

  ## SMD if requested.
  if ("SMD" %in% colnames) {
    all <- c(all, compute_smd(params$data, variable, params$strata))
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  new_output <- rbind(params$output, all, stringsAsFactors = FALSE)
  colnames(new_output) <- colnames

  return_output(params, new_output)
}

#' Get mean and SD for a particular variable.
#'
#' Does a one-way ANOVA test (t-test if only 2 groups) if the output data frame has a p value column.
#'
#' @param data A tibble (legacy mode) or a table_builder object from \code{make_output_df()} (pipe mode).
#' @param strata The factor variable to stratify by. Must be a factor (character and numeric are not accepted).
#'   \strong{In pipe mode, omit this argument} — it is read from the builder object.
#'   Passing it positionally in pipe mode will throw a clear error; use named arguments if needed.
#'   In legacy mode, this argument is required.
#' @param variable Name of the numeric variable to summarise.
#' @param name String label for this row in the output table.
#' @param output The data frame to append results to.
#'   \strong{In pipe mode, omit this argument} — it is read from the builder object.
#'   In legacy mode, this argument is required.
#' @param round Number of decimal places. In pipe mode, omit to use the default set in \code{make_output_df()}. Can be overridden per-row if needed.
#'
#' @import dplyr
#'
#' @export
get_mean_sd <- function(data, strata = NULL, variable, name, output = NULL, round = NULL) {
  # Extract parameters (handles both modes)
  params <- extract_params(data, strata, output, round)

  if (!variable %in% names(params$data)) {
    stop("Variable '", variable, "' not found in data.")
  }
  # Data can't be grouped already
  if (is_grouped_df(params$data)) {
    stop("The `data` provided is grouped. This is cause issues with later functions.
       Fix this by running `data <- ungroup(data)`")
  }
  # Strata variable needs to be a factor.
  if (!is.factor(params$data[[params$strata]])) {
    stop("Strata variable needs to be a factor")
  }
  # Variable needs to be numeric
  if (!is.numeric(params$data[[variable]])) {
    stop("Variable needs to be numeric")
  }

  # Saving the column names for later.
  colnames <- colnames(params$output)

  by_strata <- params$data %>%
    group_by(.data[[params$strata]], .drop = FALSE) %>%
    summarise(clean = paste0(
      format_num(mean(.data[[variable]], na.rm = TRUE), params$round), " (",
      format_num(sd(.data[[variable]], na.rm = TRUE), params$round), ")"
    ), .groups = "drop") %>%
    select(clean) %>%
    t()

  # Now, getting the total to put at the beginning.
  total <- params$data %>%
    summarise(clean = paste0(
      format_num(mean(.data[[variable]], na.rm = TRUE), params$round), " (",
      format_num(sd(.data[[variable]], na.rm = TRUE), params$round), ")"
    )) %>%
    select(clean) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " Mean (SD)"), total, by_strata)

  ## Doing a oneway ANOVA test.
  if ("p" %in% colnames) {
    test <- params$data %>%
      summarise(test = format_pvalue(oneway.test(.data[[variable]] ~ .data[[params$strata]])$p.value)) %>%
      select(test) %>%
      t()
    all <- c(all, test)
  }

  ## SMD if requested.
  if ("SMD" %in% colnames) {
    all <- c(all, compute_smd(params$data, variable, params$strata))
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  new_output <- rbind(params$output, all, stringsAsFactors = FALSE)
  colnames(new_output) <- colnames

  return_output(params, new_output)
}


#' Get count and percentage. By group.
#'
#' Gets count and percentage for factor variables by group.
#' NAs and empty strings are converted to a separate level "Missing"
#' Does a chisquare test if the 'output' argument has a p value column in it.
#' Unless ID variable is specified, the denominator is the number of rows in the dataset.
#' @param data A tibble (legacy mode) or a table_builder object from \code{make_output_df()} (pipe mode).
#' @param strata The factor variable to stratify by. Must be a factor (character and numeric are not accepted).
#'   \strong{In pipe mode, omit this argument} — it is read from the builder object.
#'   Passing it positionally in pipe mode will throw a clear error; use named arguments if needed.
#'   In legacy mode, this argument is required.
#' @param variable Factor variable to summarise. Non-factors are coerced to factor automatically.
#' @param name String label for this row in the output table.
#' @param output The data frame to append results to.
#'   \strong{In pipe mode, omit this argument} — it is read from the builder object.
#'   In legacy mode, this argument is required.
#' @param id Optional. Name of the ID variable to use as the denominator. Useful for repeated-measures
#'   data where the sum of numerators can exceed 100.
#' @param round Number of decimal places. In pipe mode, omit to use the default set in \code{make_output_df()}. Can be overridden per-row if needed.
#' @param sort_by_freq If TRUE, rows are sorted by descending total frequency rather than factor level order.
#' @param data_override Optionally override the data in the builder object with a different data frame. Rarely needed.
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_n_percent <- function(data, strata = NULL, variable, name, output = NULL, id = "", round = NULL,
                          sort_by_freq = FALSE, data_override = NULL) {
  params <- extract_params(data, strata, output, round, data_override)

  if (!variable %in% names(params$data)) {
    stop("Variable '", variable, "' not found in data.")
  }
  # Data can't be grouped already
  if (is_grouped_df(params$data)) {
    stop("The `data` provided is grouped. This is cause issues with later functions.
       Fix this by running `data <- ungroup(data)`")
  }
  # Variable needs to be a factor.
  if (!is.factor(params$data[[params$strata]])) {
    stop("Strata variable needs to be a factor")
  }

  # Saving the column names for later.
  colnames <- colnames(params$output)

  # Converting the variable to a factor if it isn't already.
  if (!is.factor(params$data[[variable]])) {
    params$data[[variable]] <- factor(params$data[[variable]])
  }

  # replacing NAs with missing, and putting it last.
  params$data[[variable]] <- fct_na_value_to_level(params$data[[variable]], level = "Missing")

  # Also replacing empty strings with the word 'Missing' and putting that last.
  params$data[[variable]] <- suppressWarnings(fct_recode(params$data[[variable]], Missing = ""))
  params$data[[variable]] <- suppressWarnings(fct_relevel(params$data[[variable]], "Missing", after = Inf))

  # Working out what the denominator should be. If ID is specified,
  # everything gets counted once per ID. If not, once per row.
  if (id != "") {
    denominators <- params$data %>%
      distinct(across(c(id, params$strata))) %>%
      group_by(.data[[params$strata]]) %>%
      summarise(den = n(), .groups = "drop")
    total_den <- n_distinct(params$data[[id]])
  } else {
    denominators <- params$data %>%
      group_by(.data[[params$strata]]) %>%
      summarise(den = n(), .groups = "drop")
    total_den <- nrow(params$data)
  }

  # Doing it for the strata.
  by_strata <-
    params$data %>%
    group_by(.data[[params$strata]], .data[[variable]], .drop = FALSE) %>%
    summarise(n = n(), .groups = "drop") %>%
    complete(.data[[params$strata]], .data[[variable]], fill = list(n = 0)) %>%
    left_join(denominators, by = params$strata) %>%
    group_by(.data[[params$strata]]) %>%
    mutate(perc = paste0(n, " (", format_num(100 * n / den, params$round), ")")) %>%
    select(-n, -den) %>%
    pivot_wider(names_from = all_of(params$strata), values_from = perc) %>%
    select(-all_of(variable))

  # Now the total.
  total <-
    params$data %>%
    group_by(.data[[variable]], .drop = FALSE) %>%
    summarise(n = n(), .groups = "drop") %>%
    complete(.data[[variable]], fill = list(n = 0)) %>%
    mutate(perc = paste0(n, " (", format_num(100 * n / total_den, params$round), ")"))

  # Joining them together
  all <- cbind(total, by_strata) %>%
    mutate(!!variable := as.character(.data[[variable]]))

  # Sorting by total frequency instead of default factor ordering if required.
  if (sort_by_freq) {
    all <- all %>%
      arrange(desc(n)) %>%
      select(-n)
  } else {
    all <- all %>%
      select(-n)
  }

  # Top row contains the variable name
  top_row <- c(paste0(name, " N(%)"), rep("", length(params$output) - 1))

  # Doing a chi-square test if necessary
  if ("p" %in% colnames) {
    test <- params$data %>%
      summarise(test = format_pvalue(chisq.test(.data[[variable]], .data[[params$strata]])$p.value)) %>%
      select(test) %>%
      t()

    # Overwriting the top row with the p value.
    top_row <- c(paste0(name, " N(%)"), rep("", length(params$output) - 2), test)
    # Also, the 'all' df needs an extra col for the p value
    all["p"] <- ""
  }

  # SMD if requested — uses override data since variable lives there.
  if ("SMD" %in% colnames) {
    top_row <- c(top_row, compute_smd(params$data, variable, params$strata))
    all["SMD"] <- ""
  }

  all <- rbind(top_row, all)
  # Renaming the variables to let the 2 data frames stack on top of each other.
  colnames(all) <- colnames
  new_output <- rbind(params$output, all, stringsAsFactors = FALSE)

  return_output(params, new_output)
}


#' Get sum of a numeric variable.
#' Treats missing values as zero.
#'
#' @param data A tibble (legacy mode) or a table_builder object from \code{make_output_df()} (pipe mode).
#' @param strata The factor variable to stratify by. Must be a factor (character and numeric are not accepted).
#'   \strong{In pipe mode, omit this argument} — it is read from the builder object.
#'   Passing it positionally in pipe mode will throw a clear error; use named arguments if needed.
#'   In legacy mode, this argument is required.
#' @param variable Numeric variable to sum. Missing values are treated as zero.
#' @param name String label for this row in the output table.
#' @param output The data frame to append results to.
#'   \strong{In pipe mode, omit this argument} — it is read from the builder object.
#'   In legacy mode, this argument is required.
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_sum <- function(data, strata = NULL, variable, name, output = NULL,
                    round = NULL) {
  params <- extract_params(data, strata, output, round)

  if (!variable %in% names(params$data)) {
    stop("Variable '", variable, "' not found in data.")
  }
  # Data can't be grouped already
  if (is_grouped_df(params$data)) {
    stop("The `data` provided is grouped. This is cause issues with later functions.
       Fix this by running `data <- ungroup(data)`")
  }
  # Strata variable needs to be a factor.
  if (!is.factor(params$data[[params$strata]])) {
    stop("Strata variable needs to be a factor")
  }
  # Variable needs to be numeric
  if (!is.numeric(params$data[[variable]])) {
    stop("Variable needs to be numeric")
  }

  # Saving the column names for later.
  colnames <- colnames(params$output)

  by_strata <- params$data %>%
    group_by(.data[[params$strata]], .drop = FALSE) %>%
    summarise(clean = format_num((sum(.data[[variable]], na.rm = TRUE)), params$round), .groups = "drop") %>%
    select(clean) %>%
    t()

  # Now, getting the total to put at the beginning.
  total <- params$data %>%
    summarise(clean = format_num((sum(.data[[variable]], na.rm = TRUE)), params$round)) %>%
    select(clean) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " sum"), total, by_strata)

  ## No test.
  if ("p" %in% colnames) {
    test <- params$data %>%
      summarise(test = "") %>%
      select(test) %>%
      t()
    all <- c(all, test)
  }

  ## No SMD for sums.
  if ("SMD" %in% colnames) {
    all <- c(all, "")
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  new_output <- rbind(params$output, all, stringsAsFactors = FALSE)
  colnames(new_output) <- colnames
  return_output(params, new_output)
}

#' Count number of non-missing values
#' Displays number of non-missing rows.
#'
#' @param data A tibble (legacy mode) or a table_builder object from \code{make_output_df()} (pipe mode).
#' @param strata The factor variable to stratify by. Must be a factor (character and numeric are not accepted).
#'   \strong{In pipe mode, omit this argument} — it is read from the builder object.
#'   Passing it positionally in pipe mode will throw a clear error; use named arguments if needed.
#'   In legacy mode, this argument is required.
#' @param variable Variable to count non-missing values for.
#' @param name String label for this row in the output table.
#' @param output The data frame to append results to.
#'   \strong{In pipe mode, omit this argument} — it is read from the builder object.
#'   In legacy mode, this argument is required.
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_count <- function(data, strata = NULL, variable, name, output = NULL) {
  params <- extract_params(data, strata, output)

  if (!variable %in% names(params$data)) {
    stop("Variable '", variable, "' not found in data.")
  }
  # Data can't be grouped already
  if (is_grouped_df(params$data)) {
    stop("The `data` provided is grouped. This is cause issues with later functions.
       Fix this by running `data <- ungroup(data)`")
  }
  # Strata needs to be a factor.
  if (!is.factor(params$data[[params$strata]])) {
    stop("Strata variable needs to be a factor")
  }

  # Saving the column names for later.
  colnames <- colnames(params$output)

  # Doing it for the strata.
  by_strata <-
    params$data %>%
    group_by(.data[[params$strata]], .drop = FALSE) %>%
    summarise(n = sum(!is.na(.data[[variable]])), .groups = "drop") %>%
    select(n) %>%
    t()

  # Now the total.
  total <-
    params$data %>%
    summarise(n = sum(!is.na(.data[[variable]]))) %>%
    select(n) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " N"), total, by_strata)

  ## No test.
  if ("p" %in% colnames) {
    test <- params$data %>%
      summarise(test = "") %>%
      select(test) %>%
      t()
    all <- c(all, test)
  }

  ## No SMD for counts.
  if ("SMD" %in% colnames) {
    all <- c(all, "")
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  new_output <- rbind(params$output, all, stringsAsFactors = FALSE)
  colnames(new_output) <- colnames
  return_output(params, new_output)
}

#' Count number of unique non-missing values
#' Displays number of non-missing rows.
#'
#' @param data A tibble (legacy mode) or a table_builder object from \code{make_output_df()} (pipe mode).
#' @param strata The factor variable to stratify by. Must be a factor (character and numeric are not accepted).
#'   \strong{In pipe mode, omit this argument} — it is read from the builder object.
#'   Passing it positionally in pipe mode will throw a clear error; use named arguments if needed.
#'   In legacy mode, this argument is required.
#' @param variable Variable to count unique non-missing values for.
#' @param name String label for this row in the output table.
#' @param output The data frame to append results to.
#'   \strong{In pipe mode, omit this argument} — it is read from the builder object.
#'   In legacy mode, this argument is required.
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_unique_count <- function(data, strata = NULL, variable, name, output = NULL) {
  params <- extract_params(data, strata, output)

  if (!variable %in% names(params$data)) {
    stop("Variable '", variable, "' not found in data.")
  }
  # Data can't be grouped already
  if (is_grouped_df(params$data)) {
    stop("The `data` provided is grouped. This is cause issues with later functions.
       Fix this by running `data <- ungroup(data)`")
  }
  # Strata needs to be a factor.
  if (!is.factor(params$data[[params$strata]])) {
    stop("Strata variable needs to be a factor")
  }

  # Saving the column names for later.
  colnames <- colnames(params$output)

  # Doing it for the strata.
  by_strata <-
    params$data %>%
    distinct(.data[[params$strata]], .data[[variable]], .keep_all = TRUE) %>%
    group_by(.data[[params$strata]], .drop = FALSE) %>%
    summarise(n = sum(!is.na(.data[[variable]])), .groups = "drop") %>%
    select(n) %>%
    t()

  # Now the total.
  total <-
    params$data %>%
    distinct(.data[[variable]], .keep_all = TRUE) %>%
    summarise(n = sum(!is.na(.data[[variable]]))) %>%
    select(n) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " N"), total, by_strata)

  ## No test.
  if ("p" %in% colnames) {
    test <- params$data %>%
      summarise(test = "") %>%
      select(test) %>%
      t()
    all <- c(all, test)
  }

  ## No SMD for unique counts.
  if ("SMD" %in% colnames) {
    all <- c(all, "")
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  new_output <- rbind(params$output, all, stringsAsFactors = FALSE)
  colnames(new_output) <- colnames

  return_output(params, new_output)
}

#' Count number of times a value is present in a variable
#'
#' NA values are not included in the numerator, but are included in the denominator.
#' Chi squared test. Equivalent to Z test if only 2 groups.
#'
#' @param data A tibble (legacy mode) or a table_builder object from \code{make_output_df()} (pipe mode).
#' @param strata The factor variable to stratify by. Must be a factor (character and numeric are not accepted).
#'   \strong{In pipe mode, omit this argument} — it is read from the builder object.
#'   Passing it positionally in pipe mode will throw a clear error; use named arguments if needed.
#'   In legacy mode, this argument is required.
#' @param variable Variable to check.
#' @param value The specific value to count occurrences of. Always pass this as a named argument
#'   (e.g. \code{value = 1}) to avoid ambiguity with positional argument matching.
#' @param name String label for this row in the output table.
#' @param output The data frame to append results to.
#'   \strong{In pipe mode, omit this argument} — it is read from the builder object.
#'   In legacy mode, this argument is required.
#' @param round Number of decimal places. In pipe mode, omit to use the default set in \code{make_output_df()}. Can be overridden per-row if needed.
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_n_percent_value <- function(data, strata = NULL, variable, value, name, output = NULL, round = NULL) {
  params <- extract_params(data, strata, output, round)

  if (!variable %in% names(params$data)) {
    stop("Variable '", variable, "' not found in data.")
  }
  # Data can't be grouped already
  if (is_grouped_df(params$data)) {
    stop("The `data` provided is grouped. This is cause issues with later functions.
       Fix this by running `data <- ungroup(data)`")
  }
  # Strata needs to be a factor.
  if (!is.factor(params$data[[params$strata]])) {
    stop("Strata variable needs to be a factor")
  }

  # Saving the column names for later.
  colnames <- colnames(params$output)

  # Doing it for the strata.
  by_strata <-
    params$data %>%
    group_by(.data[[params$strata]], .drop = FALSE) %>%
    summarise(
      n = sum(.data[[variable]] == value, na.rm = TRUE),
      total = n(),
      perc = paste0(n, " (", format_num(100 * n / total, params$round), ")"),
      .groups = "drop"
    ) %>%
    select(perc) %>%
    t()

  # Now the total.
  total <-
    params$data %>%
    summarise(
      n = sum(.data[[variable]] == value, na.rm = TRUE),
      total = n(),
      perc = paste0(n, " (", format_num(100 * n / total, params$round), ")")
    ) %>%
    select(perc) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " N(%)"), total, by_strata)

  ## Chi-square test if p column present.
  if ("p" %in% colnames) {
    test <- params$data %>%
      summarise(test = format_pvalue(chisq.test(.data[[variable]], .data[[params$strata]])$p.value)) %>%
      select(test) %>%
      t()

    all <- c(all, test)
  }

  ## SMD if requested.
  if ("SMD" %in% colnames) {
    all <- c(all, compute_smd(params$data, variable, params$strata))
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  new_output <- rbind(params$output, all, stringsAsFactors = FALSE)
  colnames(new_output) <- colnames

  return_output(params, new_output)
}

#' Get N(%) availability - the number of times the variable is not NA
#'
#' @param data A tibble (legacy mode) or a table_builder object from \code{make_output_df()} (pipe mode).
#' @param strata The factor variable to stratify by. Must be a factor (character and numeric are not accepted).
#'   \strong{In pipe mode, omit this argument} — it is read from the builder object.
#'   Passing it positionally in pipe mode will throw a clear error; use named arguments if needed.
#'   In legacy mode, this argument is required.
#' @param variable Variable to check availability for. Can be numeric, string, or factor.
#' @param name String label for this row in the output table.
#' @param output The data frame to append results to.
#'   \strong{In pipe mode, omit this argument} — it is read from the builder object.
#'   In legacy mode, this argument is required.
#' @param round Number of decimal places. In pipe mode, omit to use the default set in \code{make_output_df()}. Can be overridden per-row if needed.
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_availability <- function(data, strata = NULL, variable, name, output = NULL, round = NULL) {
  params <- extract_params(data, strata, output, round)

  if (!variable %in% names(params$data)) {
    stop("Variable '", variable, "' not found in data.")
  }
  # Data can't be grouped already
  if (is_grouped_df(params$data)) {
    stop("The `data` provided is grouped. This is cause issues with later functions.
       Fix this by running `data <- ungroup(data)`")
  }
  # Strata needs to be a factor.
  if (!is.factor(params$data[[params$strata]])) {
    stop("Strata variable needs to be a factor")
  }

  # Saving the column names for later.
  colnames <- colnames(params$output)

  # Doing it for the strata.
  by_strata <-
    params$data %>%
    group_by(.data[[params$strata]], .drop = FALSE) %>%
    summarise(
      n = sum(!is.na(.data[[variable]]), na.rm = TRUE),
      total = n(),
      perc = paste0(n, " (", format_num(100 * n / total, params$round), ")"),
      .groups = "drop"
    ) %>%
    select(perc) %>%
    t()

  # Now the total.
  total <-
    params$data %>%
    summarise(
      n = sum(!is.na(.data[[variable]]), na.rm = TRUE),
      total = n(),
      perc = paste0(n, " (", format_num(100 * n / total, params$round), ")")
    ) %>%
    select(perc) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " availability N(%)"), total, by_strata)

  ## No test.
  if ("p" %in% colnames) {
    test <- params$data %>%
      summarise(test = "") %>%
      select(test) %>%
      t()
    all <- c(all, test)
  }

  ## No SMD for availability.
  if ("SMD" %in% colnames) {
    all <- c(all, "")
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  new_output <- rbind(params$output, all, stringsAsFactors = FALSE)
  colnames(new_output) <- colnames
  return_output(params, new_output)
}
