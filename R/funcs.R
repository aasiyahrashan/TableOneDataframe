#' Title Get Median and IQR for a particular variable.
#'
#' Does a Mann-Whitney/K-Wallis test if there the output dataframe had a p value column.
#'
#' @param data Either a tibble OR a table_builder object from make_output_df
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work. (optional if using table_builder object)
#' @param variable name of variable to be summarised
#' @param name string name to put in the row.
#' @param output The dataframe to append the requested summary to (optional if using table_builder object)
#' @param round The number of decimal places to round to. (optional if using table_builder object)
#'
#' @import dplyr
#'
#' @export
get_median_iqr <- function(data, strata, variable, name, output, round = 2) {

  # Extract parameters (handles both modes)
  params <- extract_params(data, strata, output, round)

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

  # I get the median and IQR for whichever variable, paste them together as characters,
  # and save it all in the output dataframe

  by_strata <- params$data %>%
    group_by(get(params$strata), .drop = FALSE) %>%
    summarise(clean = paste0(
      format_num(median(get(variable), na.rm = TRUE), params$round), " (",
      format_num(quantile(get(variable), 0.25, na.rm = TRUE), params$round), " - ",
      format_num(quantile(get(variable), 0.75, na.rm = TRUE), params$round), ")"
    )) %>%
    select(clean) %>%
    t()

  # Now, getting the total to put at the beginning.
  total <- params$data %>%
    summarise(clean = paste0(
      format_num(median(get(variable), na.rm = TRUE), params$round), " (",
      format_num(quantile(get(variable), 0.25, na.rm = TRUE), params$round), " - ",
      format_num(quantile(get(variable), 0.75, na.rm = TRUE), params$round), ")"
    )) %>%
    select(clean) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " Median (IQR)"), total, by_strata)

  ## Doing a Mann whitney/K-wallis test if there is a p value column.
  if ("p" %in% colnames) {
    test <- params$data %>%
      summarise(test = format_pvalue(kruskal.test(get(variable) ~ get(params$strata))$p.value)) %>%
      select(test) %>%
      t()
    all <- c(all, test)
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  new_output <- rbind(params$output, all, stringsAsFactors = FALSE)
  colnames(new_output) <- colnames

  return_output(params, new_output)
}

#' Title Get mean and SD for a particular variable. By group.
#'
#' Does a one-way ANOVA test (t-test if only 2 groups) if there the output dataframe had a p value column.
#'
#' @param data Either a tibble OR a table_builder object from make_output_df
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work. (optional if using table_builder object)
#' @param variable name of variable to be summarised
#' @param name string name to put in the row.
#' @param output The dataframe to append the requested summary to (optional if using table_builder object)
#' @param round The number of decimal places to round results to. (optional if using table_builder object)
#'
#' @import dplyr
#'
#' @export
get_mean_sd <- function(data, strata, variable, name, output, round = 2) {

  # Extract parameters (handles both modes)
  params <- extract_params(data, strata, output, round)

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

  # I get the median and IQR for whichever variable, paste them together as characters,
  # and save it all in the output dataframe

  by_strata <- params$data %>%
    group_by(get(params$strata), .drop = FALSE) %>%
    summarise(clean = paste0(
      format_num(mean(get(variable), na.rm = TRUE), params$round), " (",
      format_num(sd(get(variable), na.rm = TRUE), params$round), ")"
    )) %>%
    select(clean) %>%
    t()

  # Now, getting the total to put at the beginning.
  total <- params$data %>%
    summarise(clean = paste0(
      format_num(mean(get(variable), na.rm = TRUE), params$round), " (",
      format_num(sd(get(variable), na.rm = TRUE), params$round), ")"
    )) %>%
    select(clean) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " Mean (SD)"), total, by_strata)

  ## Doing a oneway ANOVA test.
  if ("p" %in% colnames) {
    test <- params$data %>%
      summarise(test = format_pvalue(oneway.test(get(variable) ~ get(params$strata))$p.value)) %>%
      select(test) %>%
      t()
    all <- c(all, test)
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
#' @param data Either a tibble OR a table_builder object from make_output_df
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work. (optional if using table_builder object)
#' @param variable Ideally a factor variable. If not, gets converted to factor anyway.
#' @param name string name to put into the row
#' @param output The dataframe to append the requested summary to (optional if using table_builder object)
#' @param id Optional. Character vector containing name of variable to use when counting the denominator. Allows the sum of the numerators to be greater than 100 if there is more than one row per denominator variable.
#' @param round The number of decimal places to round results to (optional if using table_builder object)
#' @param sort_by_freq TRUE/FALSE for whether to overwrite factor ordering and sort in descending order of total frequency.
#' @param data_override Override data in table_builder object with this data frame (optional, rarely used)
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_n_percent <- function(data, strata, variable, name, output, id = "", round = 2,
                          sort_by_freq = FALSE, data_override = NULL) {

  params <- extract_params(data, strata, output, round, data_override)

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
  # everthing gets counted once per ID. If not, once per row.
  if (id != "") {
    denominators <- params$data %>%
      distinct(across(c(id, params$strata))) %>%
      group_by(get(params$strata)) %>%
      summarise(den = n())
    total_den <- n_distinct(params$data[[id]])
  } else {
    denominators <- params$data %>%
      group_by(get(params$strata)) %>%
      summarise(den = n())
    total_den <- nrow(params$data)
  }

  # Doing it for the strata.
  by_strata <-
    params$data %>%
    group_by(get(params$strata), get(variable), .drop = FALSE) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(`get(params$strata)`, `get(variable)`, fill = list(n = 0)) %>%
    left_join(denominators, by = c("get(params$strata)")) %>%
    group_by(`get(params$strata)`) %>%
    mutate(perc = paste0(n, " (", format_num(100 * n / den, params$round), ")")) %>%
    select(-n, -den) %>%
    pivot_wider(names_from = `get(params$strata)`, values_from = perc) %>%
    select(-`get(variable)`)

  # Now the total.
  total <-
    params$data %>%
    group_by(get(variable), .drop = FALSE) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(`get(variable)`, fill = list(n = 0)) %>%
    mutate(perc = paste0(n, " (", format_num(100 * n / total_den, params$round), ")"))

  # Joining them together
  all <- cbind(total, by_strata) %>%
    mutate(`get(variable)` = as.character(`get(variable)`))

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
      summarise(test = format_pvalue(chisq.test(get(variable), get(params$strata))$p.value)) %>%
      select(test) %>%
      t()

    # Overwriting the top row with the p value.
    top_row <- c(paste0(name, " N(%)"), rep("", length(params$output) - 2), test)
    # Also, the 'all' df needs an extra col for the p value
    all["p"] <- ""
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
#' @param data Either a tibble OR a table_builder object from make_output_df
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work. (Optional if using table_builder object)
#' @param variable Numeric variable to get sum for
#' @param name string name to put into the row
#' @param output The dataframe to append the requested summary to (Optional if using table_builder object)
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_sum <- function(data, strata, variable, name, output,
                    round = 2) {

  params <- extract_params(data, strata, output, round)

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

  # I get the sum for whichever variable, paste them together as characters,
  # and save it all in the output dataframe

  by_strata <- params$data %>%
    group_by(get(params$strata), .drop = FALSE) %>%
    summarise(clean = format_num((sum(get(variable), na.rm = TRUE)), params$round)) %>%
    select(clean) %>%
    t()

  # Now, getting the total to put at the beginning.
  total <- params$data %>%
    summarise(clean = format_num((sum(get(variable), na.rm = TRUE)), params$round)) %>%
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

  # Renaming the variables to let the 2 data frames stack on top of each other.
  new_output <- rbind(params$output, all, stringsAsFactors = FALSE)
  colnames(new_output) <- colnames
  return_output(params, new_output)
}

#' Count number of non-missing values
#' Displays number of non-missing rows.
#'
#' @param data Either a tibble OR a table_builder object from make_output_df
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work. (Optional if using table_builder object)
#' @param variable Ideally a factor variable. If not, gets converted to factor anyway.
#' @param name string name to put into the row
#' @param output The dataframe to append the requested summary to (Optional if using table_builder object)
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_count <- function(data, strata, variable, name, output) {

  params <- extract_params(data, strata, output)

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
    group_by(get(params$strata), .drop = FALSE) %>%
    summarise(n = sum(!is.na(get(variable)))) %>%
    select(n) %>%
    t()


  # Now the total.
  total <-
    params$data %>%
    summarise(n = sum(!is.na(get(variable)))) %>%
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

  # Renaming the variables to let the 2 data frames stack on top of each other.
  new_output <- rbind(params$output, all, stringsAsFactors = FALSE)
  colnames(new_output) <- colnames
  return_output(params, new_output)
}

#' Count number of unique non-missing values
#' Displays number of non-missing rows.
#'
#' @param data Either a tibble OR a table_builder object from make_output_df
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work. (Optional if using table_builder object)
#' @param variable Ideally a factor variable. If not, gets converted to factor anyway.
#' @param name string name to put into the row
#' @param output The dataframe to append the requested summary to (Optional if using table_builder object)
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_unique_count <- function(data, strata, variable, name, output) {

  params <- extract_params(data, strata, output)

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
    distinct(get(params$strata), get(variable), .keep_all = TRUE) %>%
    group_by(get(params$strata), .drop = FALSE) %>%
    summarise(n = sum(!is.na(get(variable)))) %>%
    select(n) %>%
    t()


  # Now the total.
  total <-
    params$data %>%
    distinct(get(variable), .keep_all = TRUE) %>%
    summarise(n = sum(!is.na(get(variable)))) %>%
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
#' @param data Either a tibble OR a table_builder object from make_output_df
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work. (Optional if using table_builder object)
#' @param variable Ideally a factor variable. If not, gets converted to factor anyway.
#' @param value Which value to count.
#' @param name string name to put into the row
#' @param output The dataframe to append the requested summary to (optional if using table_builder object)
#' @param round The number of decimal places to round results to. (optional if using table_builder object)
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_n_percent_value <- function(data, strata, variable, value, name, output, round = 2) {

  params <- extract_params(data, strata, output, round)

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
    group_by(get(params$strata), .drop = FALSE) %>%
    summarise(
      n = sum(get(variable) == value, na.rm = TRUE),
      total = n(),
      perc = paste0(n, " (", format_num(100 * n / total, params$round), ")")
    ) %>%
    select(perc) %>%
    t()


  # Now the total.
  total <-
    params$data %>%
    summarise(
      n = sum(get(variable) == value, na.rm = TRUE),
      total = n(),
      perc = paste0(n, " (", format_num(100 * n / total, params$round), ")")
    ) %>%
    select(perc) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " N(%)"), total, by_strata)

  ## No test.
  if ("p" %in% colnames) {
    # Need to create summary table for prop.test.
    test <- params$data %>%
      summarise(test = format_pvalue(chisq.test(get(variable), get(params$strata))$p.value)) %>%
      select(test) %>%
      t()

    all <- c(all, test)
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  new_output <- rbind(params$output, all, stringsAsFactors = FALSE)
  colnames(new_output) <- colnames

  return_output(params, new_output)
}

#' Get N(%) availability - the number of times the variable is not NA
#'
#' @param data Either a tibble OR a table_builder object from make_output_df
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work. (Optional if using table_builder object)
#' @param variable Variable to check availability for. Can be numeric, string, factor.
#' @param name string name to put into the row
#' @param output The dataframe to append the requested summary to (Optional if using table_builder object)
#' @param round The number of decimal places to round results to. (Optional if using table_builder object)
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_availability <- function(data, strata, variable, name, output, round = 2) {

  params <- extract_params(data, strata, output, round)

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
    group_by(get(params$strata), .drop = FALSE) %>%
    summarise(
      n = sum(!is.na(get(variable)), na.rm = TRUE),
      total = n(),
      perc = paste0(n, " (", format_num(100 * n / total, params$round), ")")
    ) %>%
    select(perc) %>%
    t()


  # Now the total.
  total <-
    params$data %>%
    summarise(
      n = sum(!is.na(get(variable)), na.rm = TRUE),
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

  # Renaming the variables to let the 2 data frames stack on top of each other.
  new_output <- rbind(params$output, all, stringsAsFactors = FALSE)
  colnames(new_output) <- colnames
  return_output(params, new_output)
}
