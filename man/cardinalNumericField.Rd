% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accessory.R
\name{cardinalNumericField}
\alias{cardinalNumericField}
\title{Make a numeric field with selectable associated cardinality (>, < ..).}
\usage{
cardinalNumericField(
  id,
  cardinal_id,
  label,
  value,
  cardinality = "<=",
  step = NA,
  min = NA,
  max = NA
)
}
\arguments{
\item{id}{ID to use for the numeric field}

\item{cardinal_id}{ID to use for the cardinality field}

\item{label}{Label}

\item{value}{Default value}

\item{cardinality}{Default cardinality}

\item{step}{Passed to \code{\link[shiny]{numericInput}}}

\item{min}{Passed to \code{\link[shiny]{numericInput}}}

\item{max}{Passed to \code{\link[shiny]{numericInput}}}
}
\value{
out An HTML tag object that can be rendered as HTML using
as.character()
}
\description{
A wrapper around \code{\link[shiny]{numericInput}}, providing an inline
label and an associated field specifying how the value should be applied,
i.e. 'greater than this value' etc.
}
