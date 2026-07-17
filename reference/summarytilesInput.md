# The UI input function of the summarytiles module

This module produces a row of summary "tiles" giving an at-a-glance
overview of the study: sample count, feature counts, assay data types,
contrasts and sample groupings. Clicking a tile opens a detail drawer
with a breakdown of that quantity. It is intended for the landing page
of an application, but can also be run standalone via
[`prepareApp()`](https://pinin4fjords.github.io/shinyngs/reference/prepareApp.md).

## Usage

``` r
summarytilesInput(id, eselist)
```

## Arguments

- id:

  Submodule namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

This input function supplies only the sidebar description used when the
module is run standalone; in a full application the tiles are placed in
a main panel via
[`summarytilesOutput()`](https://pinin4fjords.github.io/shinyngs/reference/summarytilesOutput.md).

## Examples

``` r
if (interactive()) {
  data(zhangneurons)
  summarytilesInput("summary", zhangneurons)

  # Almost certainly used via application creation

  app <- prepareApp("summarytiles", zhangneurons)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
