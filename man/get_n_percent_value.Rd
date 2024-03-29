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

\item{strata}{variable to stratify output by. Needs to be a factor to force to ordering to work.}

\item{variable}{Ideally a factor variable. If not, gets converted to factor anyway.}

\item{value}{Which value to count.}

\item{name}{string name to put into the row}

\item{output}{The dataframe to append the requested summary to}

\item{round}{The number of decimal places to round results to.}
}
\description{
NA values are not included in the numerator, but are included in the denominator.
Chi squared test. Equivalent to Z test if only 2 groups.
}
