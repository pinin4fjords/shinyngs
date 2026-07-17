# The server function of the summarytiles module

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example). It
computes a set of summary quantities from the supplied
`ExploratorySummarizedExperimentList`, renders them as a row of
clickable tiles, and shows a detail drawer for the selected tile. Tiles
for optional content (contrasts, gene sets) are only shown when that
content is present.

## Usage

``` r
summarytiles(id, eselist)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Examples

``` r
if (interactive()) {
  data(zhangneurons)
  summarytiles("summary", zhangneurons)

  # Almost certainly used via application creation

  app <- prepareApp("summarytiles", zhangneurons)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
