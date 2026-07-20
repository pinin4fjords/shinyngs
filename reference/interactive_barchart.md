# Make a grouped, stacked or overlaid bar chart with `plot_ly()`

Draws one bar trace per row of `matrix`, coloured and legended by row,
with columns along the x axis. Used by the `barplot` module, and by
[`interactive_count_barplot`](https://pinin4fjords.github.io/shinyngs/reference/interactive_count_barplot.md)
to render feature-annotation category counts.

## Usage

``` r
interactive_barchart(
  matrix,
  barmode = c("stack", "group", "overlay"),
  ylab = "",
  palette_name = COLORBLIND_PALETTE_NAME,
  title = NULL
)
```

## Arguments

- matrix:

  A matrix to plot, e.g. counts by category (row) and sample (column)

- barmode:

  'stack' (default), 'group' or 'overlay'. For 'overlay', rows are
  reordered by decreasing mean so each is more likely to be visible

- ylab:

  Y axis label

- palette_name:

  Valid R color palette name

- title:

  Plot title

## Value

output A plotly htmlwidget

## Examples

``` r
m <- matrix(1:6, nrow = 2, dimnames = list(c("up", "down"), c("s1", "s2", "s3")))
interactive_barchart(m, barmode = "stack", ylab = "Count")

{"x":{"visdat":{"29e076c23f01":["function () ","plotlyVisDat"]},"cur_data":"29e076c23f01","attrs":{"29e076c23f01":{"x":{},"y":{},"color":{},"colors":["#E69F00","#56B4E9"],"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":100,"l":60,"t":25,"r":10},"barmode":"stack","showlegend":true,"xaxis":{"domain":[0,1],"automargin":true,"title":" ","categoryorder":"array","categoryarray":["s1","s2","s3"],"type":"category"},"yaxis":{"domain":[0,1],"automargin":true,"title":"Count"},"hovermode":"closest"},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":["s1","s2","s3"],"y":[1,3,5],"type":"bar","name":"up","marker":{"color":"rgba(230,159,0,1)","line":{"color":"rgba(230,159,0,1)"}},"textfont":{"color":"rgba(230,159,0,1)"},"error_y":{"color":"rgba(230,159,0,1)"},"error_x":{"color":"rgba(230,159,0,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["s1","s2","s3"],"y":[2,4,6],"type":"bar","name":"down","marker":{"color":"rgba(86,180,233,1)","line":{"color":"rgba(86,180,233,1)"}},"textfont":{"color":"rgba(86,180,233,1)"},"error_y":{"color":"rgba(86,180,233,1)"},"error_x":{"color":"rgba(86,180,233,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
