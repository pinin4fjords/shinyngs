# The output function of the summarytiles module

This module produces a row of summary "tiles" giving an at-a-glance
overview of the study, with a detail drawer that opens beneath them.
Both are rendered server-side from the supplied
`ExploratorySummarizedExperimentList`, so this function just provides
the placeholder into which they are drawn.

## Usage

``` r
summarytilesOutput(id)
```

## Arguments

- id:

  Module namespace

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Examples

``` r
if (interactive()) {
  summarytilesOutput("summary")

  # Almost certainly used via application creation

  data(zhangneurons)
  app <- prepare_app("summarytiles", zhangneurons)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
