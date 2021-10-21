#' Title Get Median and IQR for a particular variable. By group.
#' Does a Mann-Whitney/K-Wallis test if there the output dataframe had a p value column.s
#'
#' @param data data frame containing required data
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work.
#' @param variable name of variable to be summarised
#' @param name string name to put in the row.
#' @param output
#'
#' @import tidyverse
#'
#' @export
get_median_iqr <- function(data, strata, variable, name, output) {

  # Saving the column names for later.
  colnames <- colnames(output)

  # I get the median and IQR for whichever variable, paste them together as characters,
  # and save it all in the output dataframe

  by_strata <- data %>%
    group_by(get(strata)) %>%
    summarise(clean = paste0(round(median(get(variable), na.rm = TRUE), 2), " (",
                             round(quantile(get(variable), 0.25, na.rm = TRUE), 2), " - ",
                             round(quantile(get(variable), 0.75, na.rm = TRUE), 2), ")")) %>%
    select(clean) %>%
    t()

  # Now, getting the total to put at the beginning.
  total <- data %>%
    summarise(clean = paste0(round(median(get(variable), na.rm = TRUE), 2), " (",
                             round(quantile(get(variable), 0.25, na.rm = TRUE), 2), " - ",
                             round(quantile(get(variable), 0.75, na.rm = TRUE), 2), ")")) %>%
    select(clean) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " Median (IQR)"), total, by_strata)

  ## Doing a Mann whitney/K-wallis test if there is a p value column.
  if("p" %in% colnames){
    test <- data %>%
      summarise(test = format(round(kruskal.test(get(variable) ~ get(strata))$p.value, 5),
                               nsmall = 5, scientific = FALSE, paired = FALSE)) %>%
      select(test) %>%
      t()
    print(all)
    all <- c(all, test)
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  output <- rbind(output, all, stringsAsFactors = FALSE)
  colnames(output) <- colnames
  output
}

#' Title Get mean and SD for a particular variable. By group.
#' Does a one-way ANOVA test (t-test if only 2 groups) if there the output dataframe had a p value column.
#'
#' @param data data frame containing required data
#' @param strata variable to stratify output by. Needs to be a factor to force to ordering to work.
#' @param variable name of variable to be summarised
#' @param name string name to put in the row.
#' @param output
#'
#' @import tidyverse
#'
#' @export
get_mean_sd <- function(data, strata, variable, name, output) {

  # Saving the column names for later.
  colnames <- colnames(output)

  # I get the median and IQR for whichever variable, paste them together as characters,
  # and save it all in the output dataframe

  by_strata <- data %>%
    group_by(get(strata)) %>%
    summarise(clean = paste0(round(mean(get(variable), na.rm = TRUE), 2), " (",
                             round(sd(get(variable), na.rm = TRUE), 2), ")")) %>%
    select(clean) %>%
    t()

  # Now, getting the total to put at the beginning.
  total <- data %>%
    summarise(clean = paste0(round(mean(get(variable), na.rm = TRUE), 2), " (",
                             round(sd(get(variable), na.rm = TRUE), 2), ")")) %>%
    select(clean) %>%
    t()

  # Filling the first variable in with the row label.
  all <- c(paste0(name, " Mean (SD)"), total, by_strata)

  ## Doing a oneway ANOVA test.
  if("p" %in% colnames){
    test <- data %>%
      summarise(test = format(round(oneway.test(get(variable) ~ get(strata))$p.value, 5),
                              nsmall = 5, scientific = FALSE, paired = FALSE)) %>%
      select(test) %>%
      t()
    print(all)
    all <- c(all, test)
  }

  # Renaming the variables to let the 2 data frames stack on top of each other.
  output <- rbind(output, all, stringsAsFactors = FALSE)
  colnames(output) <- colnames
  output
}


#' Gets count and percentage for factor variables by group. Does a chisquare test if the 'output' argument has a p value column in it.
#'
#' @param data
#' @param strata
#' @param variable
#' @param name
#' @param output
#'
#' @return
#' @export
#'
#' @examples
get_n_percent <- function(data, strata, variable, name, output){

  # Saving the column names for later.
  colnames <- colnames(output)

  # replacing NAs with missing, and putting it last.
  if(sum(is.na(data[, variable])) !=0) {
    levels(data[, variable]) <- c(levels(data[, variable]), "Missing")
    data[, variable][is.na(data[, variable])] = "Missing"
  }

  # Doing it for the strata.
  by_strata <-
    data %>%
    group_by(get(strata), get(variable)) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(`get(strata)`, `get(variable)`, fill = list(n=0)) %>%
    group_by(`get(strata)`) %>%
    mutate(total= sum(n),
           perc = paste0(n, " (", round(100*n/total, 2), ")")) %>%
    select(-total, -n) %>%
    pivot_wider(names_from = `get(strata)`, values_from = perc) %>%
    select(-`get(variable)`)


  # Now the total.
  total <-
    data %>%
    group_by(get(variable)) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    complete(`get(variable)`, fill = list(n=0)) %>%
    mutate(perc = paste0(n, " (", round(100*n/sum(n), 2), ")")) %>%
    select(-n)

  # Joining them together
  all <- cbind(total, by_strata) %>%
    mutate(`get(variable)` = as.character(`get(variable)`))

  # Top row contains the variable name
  top_row <- c(paste0(name, " N(%)"), rep("", length(output) - 1))

  # Doing a chi-square test if necessary
  if("p" %in% colnames){
    test <- data %>%
      summarise(test = format(round(chisq.test(get(variable), get(strata))$p.value, 5),
                              nsmall = 5, scientific = FALSE, paired = FALSE)) %>%
      select(test) %>%
      t()

    # Overwriting the top row with the p value.
    top_row <- c(paste0(name, " N(%)"), rep("", length(output) - 2), test)
    # Also, the 'all' df needs an extra col for the p value
    all['p'] <- ""
  }

  all <- rbind(top_row, all)
  # Renaming the variables to let the 2 data frames stack on top of each other.
  colnames(all) <- colnames
  output <- rbind(output, all, stringsAsFactors = FALSE)
  output
}

#' @param data
#' @param strata A factor variable to stratify by.
#' @param include_tests Should it create a space for p-values?
#'
#' @import tidyverse
#'
#' @return
#' @export
#'
#' @examples
make_output_df <- function(data, strata, include_tests = FALSE){
  # Needs to be a vector to extract the levels.
  levels <- levels(data %>%
                     pull(strata))

  if(include_tests){
    output <- data.frame(matrix(ncol = 3 + length(levels), nrow = 0))
    colnames(output) <- c("Variable", "Total", levels, "p")
  }
  else{
    output <- data.frame(matrix(ncol = 2 + length(levels), nrow = 0))
    colnames(output) <- c("Variable", "Total", levels)
  }
  output
}
