% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/volcanoplot.R
\name{volcanoplot}
\alias{volcanoplot}
\title{The server function of the \code{volcanoplot} module}
\usage{
volcanoplot(input, output, session, eselist)
}
\arguments{
\item{input}{Input object}

\item{output}{Output object}

\item{session}{Session object}

\item{eselist}{ExploratorySummarizedExperimentList object containing
ExploratorySummarizedExperiment objects}
}
\description{
This function is not called directly, but rather via callModule() (see
example). Essentially this just passes the results of \code{colData()}
applied to the specified SummarizedExperiment object to the
\code{simpletable} module
}
\examples{
callModule(differentialtable, "differentialtable", eselist)

# However, almost certainly called via application creation:

data(zhangneurons)
app <- prepareApp("volcanoplot", zhangneurons)
shinyApp(ui = app$ui, server = app$server)

}
\keyword{shiny}
