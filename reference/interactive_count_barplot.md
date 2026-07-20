# Plot counts of feature annotation rows by category, optionally split by a second categorical column, with `plot_ly()`

Tallies a categorical column from a feature annotation table (e.g.
`mcols()`/[`rowData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
of a `SummarizedExperiment`, or the data frame returned by the
`selectmatrix` module's `getAnnotation()`) and renders the counts as a
bar chart via
[`interactive_barchart`](https://pinin4fjords.github.io/shinyngs/reference/interactive_barchart.md).
Generalises the shape of the differential-expression-by-biotype plot
rendered by the nf-core/differentialabundance report to any categorical
annotation column.

## Usage

``` r
interactive_count_barplot(
  annotation,
  category,
  fill = NULL,
  barmode = c("group", "stack"),
  palette_name = COLORBLIND_PALETTE_NAME,
  title = NULL
)
```

## Arguments

- annotation:

  A data frame of feature annotation, one row per feature.

- category:

  Name of a column in `annotation` to count rows by; forms the plot's x
  axis.

- fill:

  Optional name of a second column in `annotation` to split counts by
  (bar colour/legend). Default `NULL`: a single, unsplit count per
  category.

- barmode:

  Bar mode when `fill` is specified: `"group"` (dodged bars, the
  default) or `"stack"`.

- palette_name:

  Valid R color palette name

- title:

  Plot title

## Value

output Plotly plot object

## Examples

``` r
interactive_count_barplot(
  data.frame(
    biotype = c("protein_coding", "protein_coding", "lncRNA", "lncRNA"),
    direction = c("Up", "Down", "Up", "Up")
  ),
  category = "biotype", fill = "direction"
)

{"x":{"visdat":{"29e05ebad35a":["function () ","plotlyVisDat"]},"cur_data":"29e05ebad35a","attrs":{"29e05ebad35a":{"x":{},"y":{},"color":{},"colors":["#E69F00","#56B4E9"],"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":100,"l":60,"t":25,"r":10},"title":"Counts by Biotype","barmode":"group","showlegend":true,"xaxis":{"domain":[0,1],"automargin":true,"title":" ","categoryorder":"array","categoryarray":["lncRNA","protein_coding"],"type":"category"},"yaxis":{"domain":[0,1],"automargin":true,"title":"Count"},"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":["lncRNA","protein_coding"],"y":[0,1],"type":"bar","name":"Down","marker":{"color":"rgba(230,159,0,1)","line":{"color":"rgba(230,159,0,1)"}},"textfont":{"color":"rgba(230,159,0,1)"},"error_y":{"color":"rgba(230,159,0,1)"},"error_x":{"color":"rgba(230,159,0,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["lncRNA","protein_coding"],"y":[2,1],"type":"bar","name":"Up","marker":{"color":"rgba(86,180,233,1)","line":{"color":"rgba(86,180,233,1)"}},"textfont":{"color":"rgba(86,180,233,1)"},"error_y":{"color":"rgba(86,180,233,1)"},"error_x":{"color":"rgba(86,180,233,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
