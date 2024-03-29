% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heatmap.R
\name{heatmapInput}
\alias{heatmapInput}
\title{The input function of the heatmap module}
\usage{
heatmapInput(id, eselist, type = "expression")
}
\arguments{
\item{id}{Submodule namespace}

\item{eselist}{ExploratorySummarizedExperimentList object containing
ExploratorySummarizedExperiment objects}

\item{type}{The type of heatmap that will be made. 'expression', 'samples' or
'pca'}
}
\value{
output An HTML tag object that can be rendered as HTML using
  as.character()
}
\description{
Three types of heatmaps are provided, employed in various places in the
rnaseq app (for example), and using much of the same code. Expresssion
heatmaps plot expression for samples by column and e.g. genes by row. A
samples heatmap plots samples vs samples to illustrate correlation patterns.
A pca heatmap plots the results of anova tests applied to examine the
associations between principal components and experimental variables.
}
\details{
This provides the form elements to control the heatmap display
}
\examples{
heatmapInput("heatmap", ese, group_vars, default_groupvar)

# Almost certainly used via application creation

data(zhangneurons)
app <- prepareApp("heatmap", zhangneurons)
shiny::shinyApp(ui = app$ui, server = app$server)

}
\keyword{shiny}
