% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accessory.R
\name{hiddenInput}
\alias{hiddenInput}
\title{Make a hidden input field. Handy for replacing superfluous single-value
selects etc}
\usage{
hiddenInput(id, values)
}
\arguments{
\item{id}{An HTML id}

\item{values}{The value the input should return}
}
\value{
output HTML as output by Shiny's \code{HTML()}
}
\description{
Make a hidden input field. Handy for replacing superfluous single-value
selects etc
}
\examples{
hiddenInput("myid", "iamavalue")

}
