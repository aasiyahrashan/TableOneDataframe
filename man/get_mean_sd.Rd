% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcs.R
\name{get_mean_sd}
\alias{get_mean_sd}
\title{Title Get mean and SD for a particular variable. By group.}
\usage{
get_mean_sd(data, strata, variable, name, output, round = 2)
}
\arguments{
\item{data}{tibble containing data. Can't be a grouped tibble}

\item{strata}{variable to stratify output by. Needs to be a factor to force to ordering to work.}

\item{variable}{name of variable to be summarised}

\item{name}{string name to put in the row.}

\item{output}{The dataframe to append the requested summary to}

\item{round}{The number of decimal places to round results to.}
}
\description{
Does a one-way ANOVA test (t-test if only 2 groups) if there the output dataframe had a p value column.
}
