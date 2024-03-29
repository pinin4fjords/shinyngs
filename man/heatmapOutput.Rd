% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heatmap.R
\name{heatmapOutput}
\alias{heatmapOutput}
\title{The output function of the heatmap module}
\usage{
heatmapOutput(id, type = "")
}
\arguments{
\item{id}{Submodule namespace}

\item{type}{Heatmap type: 'pca', 'samples' or 'expression'}
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
\examples{
heatmapOutput("heatmap")

# Almost certainly used via application creation

data(zhangneurons)
app <- prepareApp("heatmap", zhangneurons)
shiny::shinyApp(ui = app$ui, server = app$server)

}
\keyword{shiny}
