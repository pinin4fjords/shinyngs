% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accessory.R
\name{inlineField}
\alias{inlineField}
\title{Wrap a Shiny input so its label is displayed inline}
\usage{
inlineField(field_def, label, labelwidth = 6)
}
\arguments{
\item{field_def}{A field definition with NULL set for the label property}

\item{label}{Field label}

\item{labelwidth}{With (in units out of 12) for label}
}
\value{
output A UI definition that can be passed to the shinyUI function.
}
\description{
Wrap a Shiny input so its label is displayed inline
}
\examples{
inlineField(numericInput("foo", label = NULL, min = 0, max = 100, value = 50), "FOO")

}
