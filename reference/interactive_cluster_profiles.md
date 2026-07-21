# Plot expression profiles for a set of feature clusters with `plot_ly()`

Draws one sub-plot per cluster of a scaled expression matrix (e.g. as
produced by
[`run_clustering`](https://pinin4fjords.github.io/shinyngs/reference/run_clustering.md)),
combined with
[`plotly::subplot()`](https://rdrr.io/pkg/plotly/man/subplot.html).
Three display modes are available: individual sample lines, a
mean/median line with error bars, or a mean/median line with a shaded
error band. Summary statistics (mean, median and their spreads) are
derived internally with
[`summary_se`](https://pinin4fjords.github.io/shinyngs/reference/summary_se.md).

## Usage

``` r
interactive_cluster_profiles(
  matrices_by_cluster,
  cluster_display = c("filled_line", "sample_lines", "error_bars"),
  average_type = c("mean", "median"),
  limits = c("sd", "se", "ci"),
  colors = NULL,
  sample_order = NULL,
  max_sample_lines = 100,
  summarised_matrices_by_cluster = NULL
)
```

## Arguments

- matrices_by_cluster:

  A named list of matrices/data frames, one per cluster, features by row
  and samples by column (e.g. the input matrix split by
  `clustering$clustering` from
  [`run_clustering`](https://pinin4fjords.github.io/shinyngs/reference/run_clustering.md)).
  Names should be the cluster numbers as produced by
  [`split()`](https://rdrr.io/r/base/split.html), since `colors` is
  indexed by cluster number.

- cluster_display:

  'filled_line' (a mean/median line with a shaded error band),
  'sample_lines' (individual sample profiles) or 'error_bars' (a
  mean/median line with error bars)

- average_type:

  'mean' or 'median'

- limits:

  Which spread to show around the average: 'sd' (standard deviation),
  'se' (standard error) or 'ci' (95% confidence interval). Ignored when
  `cluster_display = "sample_lines"`

- colors:

  A vector of colors, indexed by cluster number (i.e. `colors[[2]]`
  colors the cluster named "2" in `matrices_by_cluster`). Defaults to
  [`make_color_scale`](https://pinin4fjords.github.io/shinyngs/reference/make_color_scale.md)

- sample_order:

  Character vector giving the sample (x axis) display order. Defaults to
  [`colnames()`](https://rdrr.io/r/base/colnames.html) of the first
  cluster's matrix

- max_sample_lines:

  Maximum number of sample lines drawn per cluster under
  `cluster_display = "sample_lines"`; larger clusters are randomly
  subsampled to this many rows

- summarised_matrices_by_cluster:

  Precomputed summary statistics, one element per cluster, as returned
  by
  [`summary_se`](https://pinin4fjords.github.io/shinyngs/reference/summary_se.md)
  on each element of `matrices_by_cluster`. Computed internally if not
  supplied; callers that already have this (e.g. to cache it separately
  from the display controls below) can pass it through directly

## Value

output A plotly htmlwidget

## Examples

``` r
set.seed(1)
matrices_by_cluster <- list(
  `1` = matrix(rnorm(30), nrow = 5, dimnames = list(paste0("gene", 1:5), paste0("sample", 1:6))),
  `2` = matrix(rnorm(30), nrow = 5, dimnames = list(paste0("gene", 6:10), paste0("sample", 1:6)))
)

interactive_cluster_profiles(matrices_by_cluster, cluster_display = "filled_line")

{"x":{"data":[{"name":"Cluster 1","x":["sample1","sample2","sample3","sample4","sample5","sample6"],"y":[0.12926989500457095,0.13513566758736473,0.038122971057216723,0.45956697097734395,0.08123054154716268,-0.34857702500850452],"type":"scatter","mode":"lines","line":{"color":"#E69F00"},"showlegend":true,"marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"name":"mean + SD","x":["sample1","sample2","sample3","sample4","sample5","sample6"],"y":[1.0903093200526439,0.80396987898492267,1.5369974042084269,0.92438466991826007,1.2823267764905752,0.35610520145597147],"type":"scatter","mode":"lines","line":{"color":"transparent"},"showlegend":false,"marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null},{"fillcolor":"rgba(0,100,80,0.2)","name":"mean - SD","x":["sample1","sample2","sample3","sample4","sample5","sample6"],"y":[-0.83176953004350207,-0.53369854381019322,-1.4607514620939936,-0.0052507279635721216,-1.1198656933962501,-1.0532592514729804],"type":"scatter","mode":"lines","fill":"tonexty","line":{"color":"transparent"},"showlegend":false,"marker":{"color":"rgba(44,160,44,1)","line":{"color":"rgba(44,160,44,1)"}},"error_y":{"color":"rgba(44,160,44,1)"},"error_x":{"color":"rgba(44,160,44,1)"},"xaxis":"x","yaxis":"y","frame":null},{"name":"Cluster 2","x":["sample1","sample2","sample3","sample4","sample5","sample6"],"y":[0.042539767666781171,0.1989206413440425,0.029397120627756075,0.23887624879882263,0.086171956832193211,0.20074176385688175],"type":"scatter","mode":"lines","line":{"color":"#56B4E9"},"showlegend":true,"marker":{"color":"rgba(214,39,40,1)","line":{"color":"rgba(214,39,40,1)"}},"error_y":{"color":"rgba(214,39,40,1)"},"error_x":{"color":"rgba(214,39,40,1)"},"xaxis":"x2","yaxis":"y","frame":null},{"name":"mean + SD","x":["sample1","sample2","sample3","sample4","sample5","sample6"],"y":[1.0295813784061598,0.89279546227232187,0.6118392789935756,0.89610061043369227,1.0788094968739461,1.350566360240401],"type":"scatter","mode":"lines","line":{"color":"transparent"},"showlegend":false,"marker":{"color":"rgba(148,103,189,1)","line":{"color":"rgba(148,103,189,1)"}},"error_y":{"color":"rgba(148,103,189,1)"},"error_x":{"color":"rgba(148,103,189,1)"},"xaxis":"x2","yaxis":"y","frame":null},{"fillcolor":"rgba(0,100,80,0.2)","name":"mean - SD","x":["sample1","sample2","sample3","sample4","sample5","sample6"],"y":[-0.94450184307259755,-0.49495417958423682,-0.55304503773806335,-0.418348112836047,-0.90646558320955961,-0.94908283252663739],"type":"scatter","mode":"lines","fill":"tonexty","line":{"color":"transparent"},"showlegend":false,"marker":{"color":"rgba(140,86,75,1)","line":{"color":"rgba(140,86,75,1)"}},"error_y":{"color":"rgba(140,86,75,1)"},"error_x":{"color":"rgba(140,86,75,1)"},"xaxis":"x2","yaxis":"y","frame":null}],"layout":{"xaxis":{"domain":[0,0.47999999999999998],"automargin":true,"categoryarray":["sample1","sample2","sample3","sample4","sample5","sample6"],"categoryorder":"array","title":"","type":"category","anchor":"y"},"xaxis2":{"domain":[0.52000000000000002,1],"automargin":true,"categoryarray":["sample1","sample2","sample3","sample4","sample5","sample6"],"categoryorder":"array","title":"","type":"category","anchor":"y"},"yaxis":{"domain":[0,1],"automargin":true,"title":"scaled<br />expression","anchor":"x"},"annotations":[],"shapes":[],"images":[],"margin":{"b":150,"l":60,"t":25,"r":10},"hovermode":"closest","showlegend":true},"attrs":{"29dc7cd1ede":{"name":"Cluster 1","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":[0.12926989500457095,0.13513566758736473,0.038122971057216723,0.45956697097734395,0.08123054154716268,-0.34857702500850452],"type":"scatter","mode":"lines","line":{"color":"#E69F00"},"showlegend":true,"inherit":true},"29dc7cd1ede.1":{"name":"mean + SD","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","line":{"color":"transparent"},"showlegend":false,"inherit":true},"29dc7cd1ede.2":{"name":"mean - SD","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","fill":"tonexty","fillcolor":"rgba(0,100,80,0.2)","line":{"color":"transparent"},"showlegend":false,"inherit":true},"29dc4fdc9a33":{"name":"Cluster 2","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":[0.042539767666781171,0.1989206413440425,0.029397120627756075,0.23887624879882263,0.086171956832193211,0.20074176385688175],"type":"scatter","mode":"lines","line":{"color":"#56B4E9"},"showlegend":true,"inherit":true},"29dc4fdc9a33.1":{"name":"mean + SD","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","line":{"color":"transparent"},"showlegend":false,"inherit":true},"29dc4fdc9a33.2":{"name":"mean - SD","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","fill":"tonexty","fillcolor":"rgba(0,100,80,0.2)","line":{"color":"transparent"},"showlegend":false,"inherit":true}},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"subplot":true,"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
