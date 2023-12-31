% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_method_condense.R
\docType{methods}
\name{condense}
\alias{condense}
\alias{condense-methods}
\alias{condense,operations-method}
\alias{condense,-methods}
\title{Condense operations objects into neat data frame}
\usage{
condense(
  obj,
  model = get_model(),
  resolution = c("year", "quarter", "month"),
  keep_types = TRUE,
  cashflow = FALSE,
  cf_init = 0,
  cf_names = c(begin = "Begin", end = "End"),
  years = get_period(obj, years = TRUE),
  digits = 2
)

\S4method{condense}{operations}(
  obj,
  model = get_model(),
  resolution = c("year", "quarter", "month"),
  keep_types = TRUE,
  cashflow = FALSE,
  cf_init = 0,
  cf_names = c(begin = "Begin", end = "End"),
  years = get_period(obj, years = TRUE),
  digits = 2
)
}
\arguments{
\item{obj}{An object of class \code{\link[businessPlanR:operations-class]{operations}}.}

\item{model}{A named list of named lists describing the stepwise accounting rules
for all data in in \code{obj}.}

\item{resolution}{One of \code{"month"}, \code{"quarter"}, or \code{"year"}.}

\item{keep_types}{Logical,
      whether the returned data frame should keep the intermediate results
for each relevant type of transaction. This will add a column \code{type} to the data frame.}

\item{cashflow}{Logical,
      whether the \code{model} describes a cashflow plan. If \code{TRUE},
calculations will start with the initial value as specified by \code{cf_init} and use the
result of each period as the starting value of following periods.}

\item{cf_init}{Numeric,
      used as the initial value for cashflow calculations if \code{cashflow=TRUE}; i.e.,
the first beginning cash value.}

\item{cf_names}{Character vector with two entries named \code{begin} and \code{end},
      used in the resulting
table for beginning cash and ending cash.}

\item{years}{Character (or numeric) vector defining the year(s) to be represented in the output. This is intended to be
useful for splitting up quarterly or monthly output.}

\item{digits}{Number of digits used for rounding values, disabled if set to \code{NA}.}
}
\value{
A data frame with a subset of the financial transactions of \code{obj}.
}
\description{
Uses the provided \code{model} to create a data frame from the
\code{\link[businessPlanR:operations-class]{operations}} object.
Depending on the type of data frame requestet (i.e., default or cashflow)
and the temporal resolution (month, quarter or year), various subsets of
the overall data in \code{obj} are returned.
}
