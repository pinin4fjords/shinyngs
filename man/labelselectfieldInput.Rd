% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labelselectfield.R
\name{labelselectfieldInput}
\alias{labelselectfieldInput}
\title{The input function of the \code{labelselectfield} module}
\usage{
labelselectfieldInput(id, max_items = 1, id_selection = FALSE)
}
\arguments{
\item{id}{Submodule namespace}

\item{max_items}{Maximum number of items that can be selected}

\item{id_selection}{Allow users to pick specific ID from those that relate
to the specified label? (default: FALSE)}
}
\value{
output An HTML tag object that can be rendered as HTML using
  as.character()
}
\description{
This module provides an input which allows filtering on the basis of data in
the metadata slot of an \code{ExploratorySummarizedExperiment}. It will only
be called by other modules requiring that input.
}
\details{
Where metadata is present, the user can select which field to select on, and
the value of that field (populated conditionall on field selction). If
specified, checkboxes are provided to allow selection of specific row IDs.

A \code{\link[shiny]{selectizeInput}} is used for performance reasons,
providing an autocomplete field for selecting from a list that could stretch
to thousands of entries. This would be difficult to do client-side using a
standard select field.
}
\examples{
labelselectfieldInput("myid")

}
