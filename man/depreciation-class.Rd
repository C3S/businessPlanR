% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_class_06_depreciation.R
\docType{class}
\name{depreciation,-class}
\alias{depreciation,-class}
\alias{depreciation}
\alias{depreciation-class}
\alias{initialize,depreciation-method}
\title{S4 Class depreciation}
\usage{
\S4method{initialize}{depreciation}(
  .Object,
  type,
  category,
  name,
  amount,
  obsolete,
  invest_month = format(Sys.Date(), "\%Y.\%m"),
  method = c("linear", "writedown", "sumofyears", "doubledecline"),
  valid_types = "default",
  value
)
}
\arguments{
\item{.Object}{The object to initialize.}

\item{type}{A character string defining the type of transaction as defined by \code{valid_types}.}

\item{category}{A character string, custom category for this transaction.}

\item{name}{A character string, custom name or ID for this transaction (i.e.,
      a particular asset that was purchased).}

\item{amount}{Numeric, the amount of money invested into the asset.}

\item{obsolete}{Integer value defining the period (in months) over which the value of the asset diminishes to zero.}

\item{invest_month}{Character string in \code{YYYY.MM} format,
      the month of the investment/purchase.}

\item{method}{One of the following, defining the depreciation method:
\itemize{
    \item{\code{"linear"}: }{The straight line depreciation. This is currently the only implemented option.}
    \item{\code{"writedown"}: }{The written-down value depreciation, not yet implemented.}
    \item{\code{"sumofyears"}: }{The sum-of-years depreciation, not yet implemented.}
    \item{\code{"doubledecline"}: }{The double-declining depreciation,
      not yet implemented.}
}}

\item{valid_types}{A character string,
      the model types defined by \code{\link[businessPlanR:set_types]{set_types}} to be used for validation.
If \code{"default"}, pre-defined example types are used.}

\item{value}{A valid data frame to be used as the value slot directly,
      omitting calculation via \code{amount}, \code{obsolete}, \code{invest_month}, etc.}
}
\description{
This is a special case of the generic class \code{\link[businessPlanR:transaction-class]{transaction}}.
}
\details{
In contrast to \code{\link[businessPlanR:revenue-class]{revenue}} or \code{\link[businessPlanR:expense-class]{expense}},
the time range of this class of objects is defined by details of the investment as specified. Only when used as
an aspect of an \code{\link[businessPlanR:operations-class]{operations}} class object,
      this range is adjusted
to fit that particular object.
}
\section{Slots}{

\describe{
\item{\code{type}}{A character string,
      for valid values see \code{valid_types}. You might use all valid types pre-defined for either
\code{revenue} or \code{expense},
      considering that you might be the depreciation giver or receiver.}

\item{\code{category}}{A character string, custom category for this depreciation.}

\item{\code{name}}{A character string, custom name or ID for this depreciation.}

\item{\code{value}}{Data frame containing an investment plan and allowance for depreciation balance,
      each month in a row named \code{YYYY.MM}.
The columns are \code{investment}, \code{depreciation}, and remaining \code{value}.}

\item{\code{valid_types}}{A character string, the model types defined by
\code{\link[businessPlanR:set_types]{set_types}} to be used for validation.}
}}

\section{Constructor function}{

Should you need to manually generate objects of this class, the constructor function 
\code{depreciation(...)} can be used instead of
\code{new("depreciation", ...)}.
}

\section{NA}{

Should you need to manually generate objects of this class, the constructor function 
\code{depreciation(...)} can be used instead of
\code{new("depreciation", ...)}.
}

\examples{
depreciation_printer <- depreciation(
    type="Depreciation",
    category="Office",
    name="Printer",
    amount=100,
    obsolete=36,
    invest_month="2019.04"
)

# turn depreciation object into an expense
depreciation_as_expense_printer <- as_transaction(
    depreciation_printer,
    to="expense",
    aspect="depreciation"
)
}
\keyword{classes}
