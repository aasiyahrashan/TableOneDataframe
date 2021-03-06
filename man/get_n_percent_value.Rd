% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcs.R
\name{get_n_percent_value}
\alias{get_n_percent_value}
\title{Count number of times a value is present in a variable}
\usage{
get_n_percent_value(data, strata, variable, value, name, output, round = 2)
}
\arguments{
\item{data}{tibble containing data. Can't be a grouped tibble}

\item{value}{Which value to count.}

\item{round}{The number of decimal places to round results to.}
}
\value{

}
\description{
NA values are not included in the numerator, but are included in the denominator.
Chi squared test. Equivalent to Z test if only 2 groups.
}
