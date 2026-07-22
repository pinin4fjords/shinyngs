# Make a PCA scree plot with `plot_ly()`

Plots the percent variance explained by each principal component as a
lines+markers trace, with an optional cumulative variance overlay.

## Usage

``` r
interactive_screeplot(
  fraction_explained,
  n_components = NULL,
  cumulative = FALSE,
  palette_name = COLORBLIND_PALETTE_NAME,
  title = "Scree plot",
  component_labels = NULL
)
```

## Arguments

- fraction_explained:

  Numeric vector of percent variance explained by each principal
  component, in order (e.g. from
  [`calculatePCAFractionExplained`](https://pinin4fjords.github.io/shinyngs/reference/calculatePCAFractionExplained.md),
  or the `percentVar` element returned by
  [`compile_pca_data`](https://pinin4fjords.github.io/shinyngs/reference/compile_pca_data.md)).

- n_components:

  Number of leading components to plot. Defaults to all of
  `fraction_explained`.

- cumulative:

  Boolean: add a cumulative variance explained trace?

- palette_name:

  Valid R color palette name

- title:

  Plot title

- component_labels:

  Optional character vector of length `n_components` to use as the
  x-axis categories instead of the plain `"PC1"`, `"PC2"`, ... labels.
  Used by
  [`interactive_pca_variance_heatmap`](https://pinin4fjords.github.io/shinyngs/reference/interactive_pca_variance_heatmap.md)
  to match this plot's x-categories to the percent-suffixed column
  labels of the heatmap it's stacked with.

## Value

output Plotly plot object

## Examples

``` r
interactive_screeplot(c(45, 25, 15, 10, 5))

{"x":{"visdat":{"29fd3bc2c047":["function () ","plotlyVisDat"]},"cur_data":"29fd3bc2c047","attrs":{"29fd3bc2c047":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":["PC1","PC2","PC3","PC4","PC5"],"y":[45,25,15,10,5],"type":"scatter","mode":"lines+markers","name":"% variance explained","line":{"color":"#E69F00"},"marker":{"color":"#E69F00"},"inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Scree plot","xaxis":{"domain":[0,1],"automargin":true,"title":"Principal component","type":"category","categoryorder":"array","categoryarray":["PC1","PC2","PC3","PC4","PC5"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"% variance explained","rangemode":"tozero"},"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":["PC1","PC2","PC3","PC4","PC5"],"y":[45,25,15,10,5],"type":"scatter","mode":"lines+markers","name":"% variance explained","line":{"color":"#E69F00"},"marker":{"color":"#E69F00","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
