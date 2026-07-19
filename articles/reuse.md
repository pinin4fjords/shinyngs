# Reusing components inside and outside Shiny

`shinyngs` is two things at once: a set of Shiny modules that assemble
into an interactive application, and a library of standalone plotting
functions that back those modules. The plotting functions are exported
and return plain `ggplot`/`plotly` objects, so you can call them
directly from an R script, an R Markdown or Quarto report, or a Shiny
app of your own, without building a full `shinyngs` application. This is
exactly how downstream pipelines (for example
[nf-core/differentialabundance](https://github.com/nf-core/differentialabundance))
embed `shinyngs` plots in their reports.

## The standalone plotting API

Each of these takes data in and returns a plot object out. None of them
needs a running Shiny session.

| Function | Returns | Shows |
|----|----|----|
| [`plotly_scatterplot()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_scatterplot.md), [`static_scatterplot()`](https://pinin4fjords.github.io/shinyngs/reference/static_scatterplot.md) | plotly / ggplot | Generic 2D/3D scatter (the engine behind PCA, MA, volcano, fold-change) |
| [`plotly_screeplot()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_screeplot.md) | plotly | Variance explained per principal component |
| [`plotly_pca_metadata_heatmap()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_pca_metadata_heatmap.md) | plotly | ANOVA associations between PCs and sample metadata |
| [`interactiveHeatmap()`](https://pinin4fjords.github.io/shinyngs/reference/interactiveHeatmap.md) | plotly | Clustered, annotated expression heatmap |
| [`plotly_clusteringDendrogram()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_clusteringDendrogram.md), [`clusteringDendrogram()`](https://pinin4fjords.github.io/shinyngs/reference/clusteringDendrogram.md) | plotly / ggplot | Sample clustering dendrogram |
| [`ggplot_boxplot()`](https://pinin4fjords.github.io/shinyngs/reference/ggplot_boxplot.md), [`plotly_boxplot()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_boxplot.md), [`plotly_quartiles()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_quartiles.md) | ggplot / plotly | Per-sample abundance distributions |
| [`ggplot_densityplot()`](https://pinin4fjords.github.io/shinyngs/reference/ggplot_densityplot.md), [`plotly_densityplot()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_densityplot.md) | ggplot / plotly | Abundance density curves |
| [`plotly_topgene_boxplots()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_topgene_boxplots.md), [`ggplot_topgene_boxplots()`](https://pinin4fjords.github.io/shinyngs/reference/ggplot_topgene_boxplots.md) | plotly / ggplot | Faceted boxplots of top differential features |
| [`plotly_cluster_profiles()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_cluster_profiles.md) | plotly | Feature-wise cluster expression profiles |
| [`plotly_upset()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_upset.md) | plotly | Intersections of feature sets across contrasts |
| [`plotly_barcodeplot()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_barcodeplot.md) | plotly | Gene-set enrichment barcode over a ranking |
| [`plotly_barchart()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_barchart.md), [`plotly_count_barplot()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_count_barplot.md) | plotly | Bar charts and categorical counts |
| [`plotly_illumina_control_probes()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_illumina_control_probes.md) | plotly | Illumina array control-probe QC |
| [`makeColorScale()`](https://pinin4fjords.github.io/shinyngs/reference/makeColorScale.md) | character vector | A colour-blind-safe categorical palette to colour any of the above |

See the [module and panel
catalogue](https://pinin4fjords.github.io/shinyngs/articles/modules.md)
for which app panel each of these backs.

## Calling a plot from a report

A minimal example: colour a PCA scatter by a metadata variable, using
[`compilePCAData()`](https://pinin4fjords.github.io/shinyngs/reference/compilePCAData.md)
for the coordinates and
[`makeColorScale()`](https://pinin4fjords.github.io/shinyngs/reference/makeColorScale.md)
for a consistent palette.

``` r

library(shinyngs)

pca <- compilePCAData(my_matrix)
groups <- my_metadata$treatment

plotly_scatterplot(
  x = pca$coords[, "PC1"],
  y = pca$coords[, "PC2"],
  colorby = groups,
  palette = makeColorScale(nlevels(factor(groups))),
  xlab = "PC1",
  ylab = "PC2"
)
```

The heatmap functions follow the same shape: pass a matrix (and,
optionally, a `sample_annotation` data frame) and get a widget back.

``` r

interactiveHeatmap(
  plotmatrix = my_matrix[selectVariableGenes(my_matrix, ntop = 500), ],
  sample_annotation = my_metadata["treatment"]
)
```

## Compute versus render: what `shinyngs` does and does not do

`shinyngs` is a viewer, not an analysis engine. It renders results you
have already computed. A handful of exported helpers do lightweight
preparation that the plots need, and it is fine to use them, but they
are not a statistics package:

- [`compilePCAData()`](https://pinin4fjords.github.io/shinyngs/reference/compilePCAData.md)
  — principal components for the scatter/scree plots
- [`selectVariableGenes()`](https://pinin4fjords.github.io/shinyngs/reference/selectVariableGenes.md)
  — pick the most variable features for a heatmap
- [`anova_pca_metadata()`](https://pinin4fjords.github.io/shinyngs/reference/anova_pca_metadata.md)
  — associate PCs with metadata for the association heatmap
- [`madScore()`](https://pinin4fjords.github.io/shinyngs/reference/madScore.md)
  — per-sample outlier scores
- [`cond_log2_transform_assays()`](https://pinin4fjords.github.io/shinyngs/reference/cond_log2_transform_assays.md)
  — conditional log2 transform

Differential statistics, enrichment testing and similar analysis belong
upstream (in your pipeline); pass their outputs in and let `shinyngs`
display them.

## Publication-quality output

Every interactive plot rendered inside the app carries a download button
that honours an app-wide PNG/SVG toggle, so any plot can be exported as
vector **SVG** for print. When you call the plotly functions yourself,
apply the same configuration to get a named SVG download button:

``` r

p <- plotly_scatterplot(x, y, colorby = groups)
plotly::config(
  p,
  toImageButtonOptions = list(format = "svg", filename = "pca")
)
```

## Gotcha: self-contained HTML reports

If you embed these plotly widgets in a **self-contained** HTML report
(Quarto `embed-resources: true`, or `rmarkdown` `self_contained: yes`),
the widget’s JavaScript dependencies must be inlined into the single
output file. When widgets are emitted from a loop with
`results = 'asis'`, htmlwidgets only registers the dependencies it has
seen rendered normally. If a later widget pulls in a dependency that no
earlier “primed” widget did, that dependency is missing at runtime and
the widget fails to draw.

The fix is to “prime” the report once, near the top, by rendering a
throwaway instance of each widget type (and each optional feature) you
will emit later, so its dependencies are collected into the
self-contained file. This is a property of htmlwidgets and
self-contained HTML in general, not specific to `shinyngs`, but it is
the most common surprise when moving these plots into a static report.

## Embedding the Shiny modules in your own app

The modules follow a three-function convention: a `*Input()` UI
function, a `*Output()` UI function, and a server function of the same
base name driven by
[`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html).
Add the UI pair to your layout and call the server function inside your
server, passing the `ExploratorySummarizedExperimentList` the module
should read from. The [developer
guide](https://pinin4fjords.github.io/shinyngs/articles/developer.md)
walks through this and through adding a new module.

### A note on in-widget controls

[`plotly_scatterplot()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_scatterplot.md)’s
opt-in `colorby_menu` argument adds a dropdown *inside* the widget for
switching the colouring variable. That control is meant for
standalone/report contexts where there is no surrounding Shiny UI.
Inside a `shinyngs` app the colouring variable is chosen with a Shiny
`selectInput`, so the modules deliberately leave `colorby_menu` off to
avoid two competing colour controls on screen. If you build your own app
around these functions, pick one mechanism or the other, not both.
