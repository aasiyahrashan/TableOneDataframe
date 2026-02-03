# TableOneDataframe

Create publication-ready "Table 1" summary tables for medical research with automatic statistical testing.

## Installation
```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("aasiyahrashan/TableOneDataframe")
```

## Quick Start
```r
library(TableOneDataframe)
data(mtcars)

# Prepare stratification variable
mtcars$cyl <- factor(mtcars$cyl)

# Build table with piping (recommended)
table1 <- make_output_df(mtcars, "cyl", include_tests = TRUE, round = 2) %>%
  get_median_iqr(variable = 'mpg', name = "Miles per gallon") %>%
  get_mean_sd(variable = 'hp', name = "Horsepower") %>%
  get_n_percent(variable = 'gear', name = "Number of gears") %>% 
  extract_df()

# Extract and display
knitr::kable(table1)
```

## Features

- **Stratified summaries**: Automatically creates columns for each level of a factor variable
- **Automatic statistical tests**: Mann-Whitney, Kruskal-Wallis, chi-square, ANOVA
- **Summary types**: 
  - Median (IQR)
  - Mean (SD)
  - Count and percentage
  - Availability/missingness
  - Sums and unique counts

## Available Functions

### Summary Functions for Continuous Variables
- `get_median_iqr()` - Median and interquartile range (Kruskal-Wallis test)
- `get_mean_sd()` - Mean and standard deviation (one-way ANOVA)
- `get_sum()` - Sum of values

### Summary Functions for Categorical Variables
- `get_n_percent()` - Count and percentage (chi-square test)
- `get_n_percent_value()` - Count/percentage of specific value (chi-square test)

### Count Functions
- `get_count()` - Count of non-missing values
- `get_unique_count()` - Count of unique non-missing values
- `get_availability()` - Percentage of non-missing data

## Two Usage Methods

### Piped Approach (Recommended)
```r
table <- make_output_df(data, "strata_var", include_tests = TRUE, round = 2) %>%
  get_median_iqr(variable = 'var1', name = "Variable 1") %>%
  get_mean_sd(variable = 'var2', name = "Variable 2") %>%
  get_n_percent(variable = 'var3', name = "Variable 3") %>% 
  extract_df()
```

**Benefits**: Less repetitive, cleaner code, set defaults once

### Legacy Approach
```r
output <- make_output_df(data, "strata_var", include_tests = TRUE)
output <- get_median_iqr(data, 'strata_var', 'var1', "Variable 1", output, round = 2)
output <- get_mean_sd(data, 'strata_var', 'var2', "Variable 2", output, round = 2)
output <- get_n_percent(data, 'strata_var', 'var3', "Variable 3", output, round = 2)
```

**Benefits**: Backward compatible with existing code

## Documentation

See the [package vignette](vignettes/introduction.Rmd) for detailed examples and usage guidelines.

## Requirements

- R >= 3.5.0
- dplyr
- forcats
- tidyr

## License

[Your chosen license]

## Contributing

Contributions are welcome! Please open an issue or submit a pull request.
