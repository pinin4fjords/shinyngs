% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/groupby.R
\name{groupbyInput}
\alias{groupbyInput}
\title{The UI function of the groupby module}
\usage{
groupbyInput(id, color = TRUE)
}
\arguments{
\item{id}{Submodule namespace}

\item{color}{Provide coloring functionality for groups?}
}
\value{
output An HTML tag object that can be rendered as HTML using
as.character()
}
\description{
The groupby module provides a UI element to choose from the
\code{group_vars} in a SummarizedExperment. Useful for coloring in a PCA etc
}
\examples{
groupbyInput(ns("heatmap"))

}
\keyword{shiny}
