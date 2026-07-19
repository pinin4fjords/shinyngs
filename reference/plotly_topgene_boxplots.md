# Make an interactive faceted boxplot of the top differential genes in a contrast

The `plotly` equivalent of
[`ggplot_topgene_boxplots`](https://pinin4fjords.github.io/shinyngs/reference/ggplot_topgene_boxplots.md).
One subplot is drawn per gene, with samples grouped by condition. The
beeswarm overlay is approximated using `plotly`'s native jittered box
points, since all of the (typically few) per-sample values are drawn and
shipping `ggbeeswarm`'s point layout to the browser isn't necessary
here.

## Usage

``` r
plotly_topgene_boxplots(
  assay,
  groupby,
  genes,
  annotations = NULL,
  labels = NULL,
  beeswarm = TRUE,
  ncol = NULL,
  palette = NULL,
  palette_name = COLORBLIND_PALETTE_NAME,
  expressiontype = "expression",
  should_transform = NULL
)
```

## Arguments

- assay:

  Numeric matrix, genes (rows) by samples (columns)

- groupby:

  Vector of group labels, one per column of `assay`

- genes:

  Character vector of row names of `assay` to facet on, in the order
  facets should appear. Also used to look up values in `assay`, so must
  match its row names even when `labels` is supplied.

- annotations:

  Optional named character vector keyed by the values in `genes`,
  rendered as a per-facet annotation (e.g. a q value string)

- labels:

  Optional named character vector keyed by the values in `genes`, used
  as the facet title in place of the raw gene identifier (e.g. a gene
  symbol where `genes` holds Ensembl IDs). Genes missing from `labels`
  fall back to their raw identifier. `genes` itself still drives the
  lookup into `assay` and the matching of `annotations`.

- beeswarm:

  Overlay individual points using
  [`geom_quasirandom`](https://rdrr.io/pkg/ggbeeswarm/man/geom_quasirandom.html)?

- ncol:

  Number of facet columns. Defaults to `min(3, length(genes))`

- palette:

  Palette of colours, one for each unique value of `groupby`

- palette_name:

  Valid R color palette name

- expressiontype:

  Expression type for use in y axis label

- should_transform:

  A boolean indicating if the log2 transformation should be applied. If
  TRUE, log2 transformation is applied unconditionally. If FALSE, no
  transformation is applied. If NULL, a conditional transformation based
  on threshold is applied.

## Value

output A `plotly` output

## Examples

``` r
require(airway)
data(airway, package = "airway")
mat <- assays(airway)[[1]][1:4, ]
groupby <- as.character(colData(airway)$dex)
plotly_topgene_boxplots(mat, groupby, rownames(mat))

{"x":{"data":[{"fillcolor":"#E69F00","type":"box","y":[9.4072677642447324,9.7698378436294409,10.152284842306582,9.5887146355822637],"name":"untrt","legendgroup":"untrt","boxpoints":"all","jitter":0.40000000000000002,"pointpos":0,"line":{"color":"black"},"marker":{"color":"#E69F00","line":{"color":"rgba(31,119,180,1)"}},"showlegend":true,"xaxis":"x","yaxis":"y","frame":null},{"fillcolor":"#56B4E9","type":"box","y":[8.8073549220576037,8.6724253419714952,10.032045726930809,9.1598713367783891],"name":"trt","legendgroup":"trt","boxpoints":"all","jitter":0.40000000000000002,"pointpos":0,"line":{"color":"black"},"marker":{"color":"#56B4E9","line":{"color":"rgba(255,127,14,1)"}},"showlegend":true,"xaxis":"x","yaxis":"y","frame":null},{"fillcolor":"#E69F00","type":"box","y":[0,0,0,0],"name":"untrt","legendgroup":"untrt","boxpoints":"all","jitter":0.40000000000000002,"pointpos":0,"line":{"color":"black"},"marker":{"color":"#E69F00","line":{"color":"rgba(44,160,44,1)"}},"showlegend":false,"xaxis":"x2","yaxis":"y2","frame":null},{"fillcolor":"#56B4E9","type":"box","y":[0,0,0,0],"name":"trt","legendgroup":"trt","boxpoints":"all","jitter":0.40000000000000002,"pointpos":0,"line":{"color":"black"},"marker":{"color":"#56B4E9","line":{"color":"rgba(214,39,40,1)"}},"showlegend":false,"xaxis":"x2","yaxis":"y2","frame":null},{"fillcolor":"#E69F00","type":"box","y":[8.8672787397096613,9.2784494582204822,9.1972166931100521,8.7039035734446628],"name":"untrt","legendgroup":"untrt","boxpoints":"all","jitter":0.40000000000000002,"pointpos":0,"line":{"color":"black"},"marker":{"color":"#E69F00","line":{"color":"rgba(148,103,189,1)"}},"showlegend":false,"xaxis":"x3","yaxis":"y3","frame":null},{"fillcolor":"#56B4E9","type":"box","y":[9.0084286220705803,8.5117526537673793,9.6420516929279767,8.9886846867721655],"name":"trt","legendgroup":"trt","boxpoints":"all","jitter":0.40000000000000002,"pointpos":0,"line":{"color":"black"},"marker":{"color":"#56B4E9","line":{"color":"rgba(140,86,75,1)"}},"showlegend":false,"xaxis":"x3","yaxis":"y3","frame":null},{"fillcolor":"#E69F00","type":"box","y":[8.0223678130284544,8.0389189892923021,7.936637939002571,7.8641861446542807],"name":"untrt","legendgroup":"untrt","boxpoints":"all","jitter":0.40000000000000002,"pointpos":0,"line":{"color":"black"},"marker":{"color":"#E69F00","line":{"color":"rgba(227,119,194,1)"}},"showlegend":false,"xaxis":"x4","yaxis":"y4","frame":null},{"fillcolor":"#56B4E9","type":"box","y":[7.7210991887071847,7.3575520046180838,8.3706874068072175,7.8392037880969436],"name":"trt","legendgroup":"trt","boxpoints":"all","jitter":0.40000000000000002,"pointpos":0,"line":{"color":"black"},"marker":{"color":"#56B4E9","line":{"color":"rgba(127,127,127,1)"}},"showlegend":false,"xaxis":"x4","yaxis":"y4","frame":null}],"layout":{"xaxis":{"domain":[0,0.46999999999999997],"automargin":true,"title":"ENSG00000000003","anchor":"y"},"xaxis2":{"domain":[0.53000000000000003,1],"automargin":true,"title":"ENSG00000000005","anchor":"y2"},"xaxis3":{"domain":[0,0.46999999999999997],"automargin":true,"title":"ENSG00000000419","anchor":"y3"},"xaxis4":{"domain":[0.53000000000000003,1],"automargin":true,"title":"ENSG00000000457","anchor":"y4"},"yaxis4":{"domain":[0,0.36781609195402298],"automargin":true,"title":"log2(Expression)","zeroline":false,"anchor":"x4"},"yaxis3":{"domain":[0,0.36781609195402298],"automargin":true,"title":[],"zeroline":false,"anchor":"x3"},"yaxis2":{"domain":[0.63218390804597702,1],"automargin":true,"title":[],"zeroline":false,"anchor":"x2"},"yaxis":{"domain":[0.63218390804597702,1],"automargin":true,"title":"log2(Expression)","zeroline":false,"anchor":"x"},"annotations":[],"shapes":[],"images":[],"height":870,"margin":{"b":170,"l":60,"t":30,"r":10},"hovermode":"closest","showlegend":true,"legend":{"title":{"text":"Group"},"orientation":"h","xref":"container","xanchor":"center","x":0.5,"yref":"container","yanchor":"bottom","y":0.017241379310344827}},"attrs":{"29d6383bcafd":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"box","y":[9.4072677642447324,9.7698378436294409,10.152284842306582,9.5887146355822637],"name":"untrt","legendgroup":"untrt","boxpoints":"all","jitter":0.40000000000000002,"pointpos":0,"fillcolor":"#E69F00","line":{"color":"black"},"marker":{"color":"#E69F00"},"showlegend":true,"inherit":true},"29d6383bcafd.1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"box","y":[8.8073549220576037,8.6724253419714952,10.032045726930809,9.1598713367783891],"name":"trt","legendgroup":"trt","boxpoints":"all","jitter":0.40000000000000002,"pointpos":0,"fillcolor":"#56B4E9","line":{"color":"black"},"marker":{"color":"#56B4E9"},"showlegend":true,"inherit":true},"29d62f6c8408":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"box","y":[0,0,0,0],"name":"untrt","legendgroup":"untrt","boxpoints":"all","jitter":0.40000000000000002,"pointpos":0,"fillcolor":"#E69F00","line":{"color":"black"},"marker":{"color":"#E69F00"},"showlegend":false,"inherit":true},"29d62f6c8408.1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"box","y":[0,0,0,0],"name":"trt","legendgroup":"trt","boxpoints":"all","jitter":0.40000000000000002,"pointpos":0,"fillcolor":"#56B4E9","line":{"color":"black"},"marker":{"color":"#56B4E9"},"showlegend":false,"inherit":true},"29d6324d137a":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"box","y":[8.8672787397096613,9.2784494582204822,9.1972166931100521,8.7039035734446628],"name":"untrt","legendgroup":"untrt","boxpoints":"all","jitter":0.40000000000000002,"pointpos":0,"fillcolor":"#E69F00","line":{"color":"black"},"marker":{"color":"#E69F00"},"showlegend":false,"inherit":true},"29d6324d137a.1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"box","y":[9.0084286220705803,8.5117526537673793,9.6420516929279767,8.9886846867721655],"name":"trt","legendgroup":"trt","boxpoints":"all","jitter":0.40000000000000002,"pointpos":0,"fillcolor":"#56B4E9","line":{"color":"black"},"marker":{"color":"#56B4E9"},"showlegend":false,"inherit":true},"29d644205830":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"box","y":[8.0223678130284544,8.0389189892923021,7.936637939002571,7.8641861446542807],"name":"untrt","legendgroup":"untrt","boxpoints":"all","jitter":0.40000000000000002,"pointpos":0,"fillcolor":"#E69F00","line":{"color":"black"},"marker":{"color":"#E69F00"},"showlegend":false,"inherit":true},"29d644205830.1":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"box","y":[7.7210991887071847,7.3575520046180838,8.3706874068072175,7.8392037880969436],"name":"trt","legendgroup":"trt","boxpoints":"all","jitter":0.40000000000000002,"pointpos":0,"fillcolor":"#56B4E9","line":{"color":"black"},"marker":{"color":"#56B4E9"},"showlegend":false,"inherit":true}},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"subplot":true,"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
