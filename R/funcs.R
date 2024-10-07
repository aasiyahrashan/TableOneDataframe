#' Title Get Median and IQR for a particular variable.
#'
#' Does a Mann-Whitney/K-Wallis test if there the output dataframe had a p value column.
#'
#' @param data tibble containing data. Can't be a grouped tibble
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work.
#' @param variable name of variable to be summarised
#' @param name string name to put in the row.
#' @param output The dataframe to append the requested summary to
#' @param round The number of decimal places to round to.
#'
#' @import dplyr
#'
#' @export
get_median_iqr <- function(data, strata, variable, name, output, round = 2) {
  # Strata variable needs to be a factor.
  if (!is.factor(data[[strata]])) {
    stop("Strata variable needs to be a factor")
  }
  # Variable needs to be numeric
  if (!is.numeric(data[[variable]])) {
    stop("Variable needs to be numeric")
  }

  # Saving the column names for later.
  colnames <- colnames(output)

  # I get the median and IQR for whichever variable, paste them together as characters,
  # and save it all in the output dataframe

  by_strata <- data %>%
    group_by(get(strata), .drop = FALSE) %>%
    summarise(clean = paste0(
      round(median(get(variable), na.rm = TRUE), round), " (",
      round(quantile(get(variable), 0.25, na.rm = TRUE), round), " - ",
      round(quantile(get(variable), 0.75, na.rm = TRUE), round), ")"
    )) %>%
    select(clean) %>%
    t()

  # Now, getting the total to put at the beginning.
  total <- data %>%
    summarise(clean = paste0(
      round(median(get(variable), na.rm = TRUE), round), " (",
      round(quantile(get(variable), 0.25, na.rm = TRUE), round), " - ",
      round(quantile(get(variable), 0.75, na.rm = TRUE), round), ")"
    )) %>%
    select(clean) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " Median (IQR)"), total, by_strata)

  ## Doing a Mann whitney/K-wallis test if there is a p value column.
  if ("p" %in% colnames) {
    test <- data %>%
      summarise(test = format(round(kruskal.test(get(variable) ~ get(strata))$p.value, 3),
        nsmall = 3, scientific = FALSE, paired = FALSE
      )) %>%
      select(test) %>%
      t()
    all <- c(all, test)
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  output <- rbind(output, all, stringsAsFactors = FALSE)
  colnames(output) <- colnames
  output
}

#' Title Get mean and SD for a particular variable. By group.
#'
#' Does a one-way ANOVA test (t-test if only 2 groups) if there the output dataframe had a p value column.
#'
#' @param data tibble containing data. Can't be a grouped tibble
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work.
#' @param variable name of variable to be summarised
#' @param name string name to put in the row.
#' @param output The dataframe to append the requested summary to
#' @param round The number of decimal places to round results to.
#'
#' @import dplyr
#'
#' @export
get_mean_sd <- function(data, strata, variable, name, output, round = 2) {
  # Strata variable needs to be a factor.
  if (!is.factor(data[[strata]])) {
    stop("Strata variable needs to be a factor")
  }
  # Variable needs to be numeric
  if (!is.numeric(data[[variable]])) {
    stop("Variable needs to be numeric")
  }

  # Saving the column names for later.
  colnames <- colnames(output)

  # I get the median and IQR for whichever variable, paste them together as characters,
  # and save it all in the output dataframe

  by_strata <- data %>%
    group_by(get(strata), .drop = FALSE) %>%
    summarise(clean = paste0(
      round(mean(get(variable), na.rm = TRUE), round), " (",
      round(sd(get(variable), na.rm = TRUE), round), ")"
    )) %>%
    select(clean) %>%
    t()

  # Now, getting the total to put at the beginning.
  total <- data %>%
    summarise(clean = paste0(
      round(mean(get(variable), na.rm = TRUE), round), " (",
      round(sd(get(variable), na.rm = TRUE), round), ")"
    )) %>%
    select(clean) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " Mean (SD)"), total, by_strata)

  ## Doing a oneway ANOVA test.
  if ("p" %in% colnames) {
    test <- data %>%
      summarise(test = format(round(oneway.test(get(variable) ~ get(strata))$p.value, 3),
        nsmall = 3, scientific = FALSE, paired = FALSE
      )) %>%
      select(test) %>%
      t()
    all <- c(all, test)
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  output <- rbind(output, all, stringsAsFactors = FALSE)
  colnames(output) <- colnames
  output
}


#' Get count and percentage. By group.
#'
#' Gets count and percentage for factor variables by group.
#' NAs and empty strings are converted to a separate level "Missing"
#' Does a chisquare test if the 'output' argument has a p value column in it.
#' Unless ID variable is specified, the denominator is the number of rows in the dataset.
#' @param data tibble containing data. Can't be a grouped tibble
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work.
#' @param variable Ideally a factor variable. If not, gets converted to factor anyway.
#' @param name string name to put into the row
#' @param output The dataframe to append the requested summary to
#' @param id Optional. Character vector containing name of variable to use when counting the denominator. Allows the sum of the numerators to be greater than 100 if there is more than one row per denominator variable.
#' @param round The number of decimal places to round results to
#' @param sort_by_freq TRUE/FALSE for whether to overwrite factor ordering and sort in descending order of total frequency.
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_n_percent <- function(data, strata, variable, name, output, id = "", round = 2,
                          sort_by_freq = FALSE) {
  # Variable needs to be a factor.
  if (!is.factor(data[[strata]])) {
    stop("Strata variable needs to be a factor")
  }

  # Saving the column names for later.
  colnames <- colnames(output)

  # Converting the variable to a factor if it isn't already.
  if (!is.factor(data[[variable]])) {
    data[[variable]] <- factor(data[[variable]])
  }

  # replacing NAs with missing, and putting it last.
  data[[variable]] <- fct_na_value_to_level(data[[variable]], level = "Missing")

  # Also replacing empty strings with the word 'Missing' and putting that last.
  data[[variable]] <- suppressWarnings(fct_recode(data[[variable]], Missing = ""))
  data[[variable]] <- suppressWarnings(fct_relevel(data[[variable]], "Missing", after = Inf))

  # Working out what the denominator should be. If ID is specified,
  # everthing gets counted once per ID. If not, once per row.
  if (id != "") {
    denominators <- data %>%
      distinct(across(c(id, strata))) %>%
      group_by(get(strata)) %>%
      summarise(den = n())
    total_den <- n_distinct(data[[id]])
  } else {
    denominators <- data %>%
      group_by(get(strata)) %>%
      summarise(den = n())
    total_den <- nrow(data)
  }

  # Doing it for the strata.
  by_strata <-
    data %>%
    group_by(get(strata), get(variable), .drop = FALSE) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(`get(strata)`, `get(variable)`, fill = list(n = 0)) %>%
    left_join(denominators, by = c("get(strata)")) %>%
    group_by(`get(strata)`) %>%
    mutate(perc = paste0(n, " (", round(100 * n / den, round), ")")) %>%
    select(-n, -den) %>%
    pivot_wider(names_from = `get(strata)`, values_from = perc) %>%
    select(-`get(variable)`)

  # Now the total.
  total <-
    data %>%
    group_by(get(variable), .drop = FALSE) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(`get(variable)`, fill = list(n = 0)) %>%
    mutate(perc = paste0(n, " (", round(100 * n / total_den, round), ")"))

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
  top_row <- c(paste0(name, " N(%)"), rep("", length(output) - 1))

  # Doing a chi-square test if necessary
  if ("p" %in% colnames) {
    test <- data %>%
      summarise(test = format(round(chisq.test(get(variable), get(strata))$p.value, 3),
        nsmall = 3, scientific = FALSE, paired = FALSE
      )) %>%
      select(test) %>%
      t()

    # Overwriting the top row with the p value.
    top_row <- c(paste0(name, " N(%)"), rep("", length(output) - 2), test)
    # Also, the 'all' df needs an extra col for the p value
    all["p"] <- ""
  }

  all <- rbind(top_row, all)
  # Renaming the variables to let the 2 data frames stack on top of each other.
  colnames(all) <- colnames
  output <- rbind(output, all, stringsAsFactors = FALSE)
  output
}


#' Get sum of a numeric variable.
#' Treats missing values as zero.
#'
#' @param data tibble containing data. Can't be a grouped tibble
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work.
#' @param variable Numeric variable to get sum for
#' @param name string name to put into the row
#' @param output The dataframe to append the requested summary to
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_sum <- function(data, strata, variable, name, output,
                    round = 2) {
  # Strata variable needs to be a factor.
  if (!is.factor(data[[strata]])) {
    stop("Strata variable needs to be a factor")
  }
  # Variable needs to be numeric
  if (!is.numeric(data[[variable]])) {
    stop("Variable needs to be numeric")
  }

  # Saving the column names for later.
  colnames <- colnames(output)

  # I get the sum for whichever variable, paste them together as characters,
  # and save it all in the output dataframe

  by_strata <- data %>%
    group_by(get(strata), .drop = FALSE) %>%
    summarise(clean = round((sum(get(variable), na.rm = TRUE)), round)) %>%
    select(clean) %>%
    t()

  # Now, getting the total to put at the beginning.
  total <- data %>%
    summarise(clean = round((sum(get(variable), na.rm = TRUE)), round)) %>%
    select(clean) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " sum"), total, by_strata)

  ## No test.
  if ("p" %in% colnames) {
    test <- data %>%
      summarise(test = "") %>%
      select(test) %>%
      t()
    all <- c(all, test)
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  output <- rbind(output, all, stringsAsFactors = FALSE)
  colnames(output) <- colnames
  output
}

#' Count number of non-missing values
#' Displays number of non-missing rows.
#'
#' @param data tibble containing data. Can't be a grouped tibble
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work.
#' @param variable Ideally a factor variable. If not, gets converted to factor anyway.
#' @param name string name to put into the row
#' @param output The dataframe to append the requested summary to
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_count <- function(data, strata, variable, name, output) {
  # Strata needs to be a factor.
  if (!is.factor(data[[strata]])) {
    stop("Strata variable needs to be a factor")
  }

  # Saving the column names for later.
  colnames <- colnames(output)

  # Doing it for the strata.
  by_strata <-
    data %>%
    group_by(get(strata), .drop = FALSE) %>%
    summarise(n = sum(!is.na(get(variable)))) %>%
    select(n) %>%
    t()


  # Now the total.
  total <-
    data %>%
    summarise(n = sum(!is.na(get(variable)))) %>%
    select(n) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " N"), total, by_strata)

  ## No test.
  if ("p" %in% colnames) {
    test <- data %>%
      summarise(test = "") %>%
      select(test) %>%
      t()
    all <- c(all, test)
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  output <- rbind(output, all, stringsAsFactors = FALSE)
  colnames(output) <- colnames
  output
}

#' Count number of unique non-missing values
#' Displays number of non-missing rows.
#'
#' @param data tibble containing data. Can't be a grouped tibble
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work.
#' @param variable Ideally a factor variable. If not, gets converted to factor anyway.
#' @param name string name to put into the row
#' @param output The dataframe to append the requested summary to
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_unique_count <- function(data, strata, variable, name, output) {
  # Strata needs to be a factor.
  if (!is.factor(data[[strata]])) {
    stop("Strata variable needs to be a factor")
  }

  # Saving the column names for later.
  colnames <- colnames(output)

  # Doing it for the strata.
  by_strata <-
    data %>%
    distinct(get(strata), get(variable), .keep_all = TRUE) %>%
    group_by(get(strata), .drop = FALSE) %>%
    summarise(n = sum(!is.na(get(variable)))) %>%
    select(n) %>%
    t()


  # Now the total.
  total <-
    data %>%
    distinct(get(variable), .keep_all = TRUE) %>%
    summarise(n = sum(!is.na(get(variable)))) %>%
    select(n) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " N"), total, by_strata)

  ## No test.
  if ("p" %in% colnames) {
    test <- data %>%
      summarise(test = "") %>%
      select(test) %>%
      t()
    all <- c(all, test)
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  output <- rbind(output, all, stringsAsFactors = FALSE)
  colnames(output) <- colnames
  output
}

#' Count number of times a value is present in a variable
#'
#' NA values are not included in the numerator, but are included in the denominator.
#' Chi squared test. Equivalent to Z test if only 2 groups.
#'
#' @param data tibble containing data. Can't be a grouped tibble
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work.
#' @param variable Ideally a factor variable. If not, gets converted to factor anyway.
#' @param value Which value to count.
#' @param name string name to put into the row
#' @param output The dataframe to append the requested summary to
#' @param round The number of decimal places to round results to.
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_n_percent_value <- function(data, strata, variable, value, name, output, round = 2) {
  # Strata needs to be a factor.
  if (!is.factor(data[[strata]])) {
    stop("Strata variable needs to be a factor")
  }

  # Saving the column names for later.
  colnames <- colnames(output)

  # Doing it for the strata.
  by_strata <-
    data %>%
    group_by(get(strata), .drop = FALSE) %>%
    summarise(
      n = sum(get(variable) == value, na.rm = TRUE),
      total = n(),
      perc = paste0(n, " (", round(100 * n / total, round), ")")
    ) %>%
    select(perc) %>%
    t()


  # Now the total.
  total <-
    data %>%
    summarise(
      n = sum(get(variable) == value, na.rm = TRUE),
      total = n(),
      perc = paste0(n, " (", round(100 * n / total, round), ")")
    ) %>%
    select(perc) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " N(%)"), total, by_strata)

  ## No test.
  if ("p" %in% colnames) {
    # Need to create summary table for prop.test.
    test <- data %>%
      summarise(test = format(round(chisq.test(get(variable), get(strata))$p.value, 3),
        nsmall = 3, scientific = FALSE, paired = FALSE
      )) %>%
      select(test) %>%
      t()

    all <- c(all, test)
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  output <- rbind(output, all, stringsAsFactors = FALSE)
  colnames(output) <- colnames
  output
}
#' Make output dataframe
#'
#' Includes neat column names with counts of observations in each group.
#' @param data tibble containing data. Can't be a grouped tibble. Also, can't have a variable named 'variable'.
#' @param strata A factor variable to stratify by.
#' @param include_tests Should it create a space for p-values?
#' @import dplyr
#' @export
make_output_df <- function(data, strata, include_tests = FALSE) {
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
    mutate(names = paste0(`get(strata)`, " (N=", n, ")"))

  total_name <- paste0("Total", " (N=", nrow(data), ")")

  if (include_tests) {
    output <- data.frame(matrix(ncol = 3 + length(levels), nrow = 0))
    colnames(output) <- c("Variable", total_name, strata_names$names, "p")
  } else {
    output <- data.frame(matrix(ncol = 2 + length(levels), nrow = 0))
    colnames(output) <- c("Variable", total_name, strata_names$names)
  }
  output
}

#' Get N(%) availability - the number of times the variable is not NA
#'
#' @param data tibble containing data. Can't be a grouped tibble
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work.
#' @param variable Variable to check availability for. Can be numeric, string, factor.
#' @param name string name to put into the row
#' @param output The dataframe to append the requested summary to
#' @param round The number of decimal places to round results to.
#'
#' @import dplyr
#' @import forcats
#' @import tidyr
#' @export
get_availability <- function(data, strata, variable, name, output, round = 2) {
  # Strata needs to be a factor.
  if (!is.factor(data[[strata]])) {
    stop("Strata variable needs to be a factor")
  }

  # Saving the column names for later.
  colnames <- colnames(output)

  # Doing it for the strata.
  by_strata <-
    data %>%
    group_by(get(strata), .drop = FALSE) %>%
    summarise(
      n = sum(!is.na(get(variable)), na.rm = TRUE),
      total = n(),
      perc = paste0(n, " (", round(100 * n / total, round), ")")
    ) %>%
    select(perc) %>%
    t()


  # Now the total.
  total <-
    data %>%
    summarise(
      n = sum(!is.na(get(variable)), na.rm = TRUE),
      total = n(),
      perc = paste0(n, " (", round(100 * n / total, round), ")")
    ) %>%
    select(perc) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " availability N(%)"), total, by_strata)

  ## No test.
  if ("p" %in% colnames) {
    test <- data %>%
      summarise(test = "") %>%
      select(test) %>%
      t()
    all <- c(all, test)
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  output <- rbind(output, all, stringsAsFactors = FALSE)
  colnames(output) <- colnames
  output
}
