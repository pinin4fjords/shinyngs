% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boxplot.R
\name{plotly_densityplot}
\alias{plotly_densityplot}
\title{Make a dynamic density plot with plotly}
\usage{
plotly_densityplot(
  plotmatrices,
  experiment,
  colorby = NULL,
  palette = NULL,
  expressiontype = "expression",
  palette_name = "Set1",
  annotate_samples = FALSE,
  should_transform = NULL
)
}
\arguments{
\item{plotmatrices}{Expression/ other data matrix, or named list thereof}

\item{experiment}{Annotation for the columns of plotmatrix}

\item{colorby}{Column name in \code{experiment} specifying how lines should be colored}

\item{palette}{Palette of colors, one for each unique value derived from
\code{colorby}.}

\item{expressiontype}{Expression type for use in y axis label}

\item{palette_name}{Valid R color palette name}

\item{annotate_samples}{Add a suffix to sample labels reflecting their group?}

\item{should_transform}{A boolean indicating if the log2 transformation should be applied.
If TRUE, log2 transformation is applied unconditionally.
If FALSE, no transformation is applied.
If NULL, a conditional transformation based on threshold is applied.}
}
\value{
output A \code{plotly} output
}
\description{
A simple function using \code{plotly} to make a sample density plot.
}
