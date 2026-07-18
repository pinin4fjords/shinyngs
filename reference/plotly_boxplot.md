# Make an interactive boxplot with coloring by experimental variable

Draws a `plotly` box plot of the value distribution in each sample. Box
statistics (quartiles, whiskers, outliers) are computed server-side and
supplied to plotly as precomputed values, so the browser payload scales
with the number of samples rather than the size of the matrix. This
makes it usable on full expression matrices where sending every point
would not be.

## Usage

``` r
plotly_boxplot(
  plotmatrices,
  experiment,
  colorby = NULL,
  palette = NULL,
  expressiontype = "expression",
  palette_name = COLORBLIND_PALETTE_NAME,
  whisker_distance = 1.5,
  annotate_samples = FALSE,
  should_transform = NULL,
  max_outliers = 500,
  hidden_groups = character(0),
  source = NULL
)
```

## Arguments

- plotmatrices:

  Expression/ other data matrix, or named list thereof

- experiment:

  Annotation for the columns of plotmatrix

- colorby:

  Column name in `experiment` specifying how boxes should be colored

- palette:

  Palette of colors, one for each unique value derived from `colorby`.

- expressiontype:

  Expression type for use in y axis label

- palette_name:

  Valid R color palette name

- whisker_distance:

  IQR multiplier for the whiskers, and the boundary beyond which points
  are drawn as outliers (see `coef` in
  [`geom_boxplot`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html),
  default: 1.5)

- annotate_samples:

  Add a suffix to sample labels reflecting their group?

- should_transform:

  A boolean indicating if the log2 transformation should be applied. If
  TRUE, log2 transformation is applied unconditionally. If FALSE, no
  transformation is applied. If NULL, a conditional transformation based
  on threshold is applied.

- max_outliers:

  Maximum number of outlier points to draw per facet. When a sample set
  produces more, the most extreme (furthest from the median) are kept so
  the overlay stays bounded (default: 500).

- hidden_groups:

  Values of `colorby` to exclude: their samples are dropped so the plot
  is redrawn on the remainder, while they stay in the legend (as
  `legendonly`) so they can be toggled back on.

- source:

  Optional plotly source id used to route legend-click
  (`plotly_restyle`) events back to a Shiny session.

## Value

output A `plotly` output

## Examples

``` r
require(airway)
data(airway, package = "airway")
plotly_boxplot(assays(airway)[[1]], data.frame(colData(airway)), colorby = "dex")

{"x":{"visdat":{"29f25fc6b76":["function () ","plotlyVisDat"]},"cur_data":"29f25fc6b76","attrs":{"29f25fc6b76":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"box","x":["SRR1039508","SRR1039512","SRR1039516","SRR1039520"],"lowerfence":[0,0,0,0],"q1":[2,2,2,2],"median":[5.8579809951275719,5.7279204545631988,5.6724253419714952,5.7548875021634682],"q3":[9.0195907283578816,9.1163439612374688,9.1421070573025496,8.8634113349895589],"upperfence":[18.184497655033567,18.970751893179973,18.601651110079935,18.531206291595154],"name":"untrt","legendgroup":"untrt","fillcolor":"#E69F00","line":{"color":"black"},"visible":true,"showlegend":true,"inherit":true},"29f25fc6b76.1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"box","x":["SRR1039509","SRR1039513","SRR1039517","SRR1039521"],"lowerfence":[0,0,0,0],"q1":[1.5849625007211561,2,2,2],"median":[5.6147098441152083,5.7004397181410917,5.7004397181410917,5.9068905956085187],"q3":[8.8703647195834048,8.6008412178868934,9.3685064615076925,9.014020470314934],"upperfence":[17.963878217786878,18.063173856706324,18.615180592625467,18.506838296307528],"name":"trt","legendgroup":"trt","fillcolor":"#56B4E9","line":{"color":"black"},"visible":true,"showlegend":true,"inherit":true}},"layout":{"margin":{"b":120,"l":60,"t":25,"r":10},"xaxis":{"domain":[0,1],"automargin":true,"title":[],"categoryorder":"array","categoryarray":["SRR1039508","SRR1039512","SRR1039516","SRR1039520","SRR1039509","SRR1039513","SRR1039517","SRR1039521"],"type":"category"},"yaxis":{"domain":[0,1],"automargin":true,"title":"log2(Expression)","zeroline":false},"legend":{"title":{"text":"Dex"},"orientation":"h","xanchor":"center","x":0.5,"y":-0.29999999999999999},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"fillcolor":"#E69F00","type":"box","x":["SRR1039508","SRR1039512","SRR1039516","SRR1039520"],"lowerfence":[0,0,0,0],"q1":[2,2,2,2],"median":[5.8579809951275719,5.7279204545631988,5.6724253419714952,5.7548875021634682],"q3":[9.0195907283578816,9.1163439612374688,9.1421070573025496,8.8634113349895589],"upperfence":[18.184497655033567,18.970751893179973,18.601651110079935,18.531206291595154],"name":"untrt","legendgroup":"untrt","line":{"color":"black"},"visible":true,"showlegend":true,"marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"xaxis":"x","yaxis":"y","frame":null},{"fillcolor":"#56B4E9","type":"box","x":["SRR1039509","SRR1039513","SRR1039517","SRR1039521"],"lowerfence":[0,0,0,0],"q1":[1.5849625007211561,2,2,2],"median":[5.6147098441152083,5.7004397181410917,5.7004397181410917,5.9068905956085187],"q3":[8.8703647195834048,8.6008412178868934,9.3685064615076925,9.014020470314934],"upperfence":[17.963878217786878,18.063173856706324,18.615180592625467,18.506838296307528],"name":"trt","legendgroup":"trt","line":{"color":"black"},"visible":true,"showlegend":true,"marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
