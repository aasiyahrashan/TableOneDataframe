% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcs.R
\name{get_median_iqr}
\alias{get_median_iqr}
\title{Title Get Median and IQR for a particular variable.}
\usage{
get_median_iqr(data, strata, variable, name, output, round = 2)
}
\arguments{
\item{data}{tibble containing data. Can't be a grouped tibble}

\item{strata}{variable to stratify output by. Needs to be a factor to force to ordering to work.}

\item{variable}{name of variable to be summarised}

\item{name}{string name to put in the row.}

\item{output}{The dataframe to append the requested summary to}

\item{round}{The number of decimal places to round to.}
}
\description{
Does a Mann-Whitney/K-Wallis test if there the output dataframe had a p value column.
}
