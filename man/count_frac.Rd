% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_frac.R
\name{count_frac}
\alias{count_frac}
\title{Create percentages within group}
\usage{
count_frac(data, ...)
}
\arguments{
\item{data}{Input data.}

\item{...}{variables. Last one is the one we do not group_by.}
}
\value{
A tibble
}
\description{
One-line solution for count -> group_by -> mutate -> ungroup.
}
