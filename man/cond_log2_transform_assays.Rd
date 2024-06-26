% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accessory.R
\name{cond_log2_transform_assays}
\alias{cond_log2_transform_assays}
\title{Conditionally apply log2 transformation on assay data based on log2_assays parameter.}
\usage{
cond_log2_transform_assays(
  assay_data,
  log2_assays,
  threshold = 30,
  reverse = FALSE,
  prettify_names = TRUE
)
}
\arguments{
\item{assay_data}{A list containing matrices as assay data.}

\item{log2_assays}{A string parameter that can be NULL, empty, or a non-empty string.
If NULL: log2 transformation will be guessed based on input assays.
If empty: no log2 transformation will be applied.
If non-empty: log2 transformation will be applied unconditionally to specified assays.}

\item{threshold}{A numeric threshold to determine if the matrix should be log-transformed.
This is only checked if should_transform is NULL.}

\item{reverse}{Boolean, should we unlog rather than log?}

\item{prettify_names}{Boolean. Prettify element names? Passed to validate_indices().}
}
\value{
A modified assay_data list.
}
\description{
Conditionally apply log2 transformation on assay data based on log2_assays parameter.
}
