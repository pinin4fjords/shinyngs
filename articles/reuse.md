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
| [`interactive_scatterplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_scatterplot.md), [`static_scatterplot()`](https://pinin4fjords.github.io/shinyngs/reference/static_scatterplot.md) | plotly / ggplot | Generic 2D/3D scatter (the engine behind PCA, MA, volcano, fold-change) |
| [`interactive_screeplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_screeplot.md) | plotly | Variance explained per principal component |
| [`interactive_pca_metadata_heatmap()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_pca_metadata_heatmap.md) | plotly | ANOVA associations between PCs and sample metadata |
| [`interactive_heatmap()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_heatmap.md) | plotly | Clustered, annotated expression heatmap |
| [`interactive_clustering_dendrogram()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_clustering_dendrogram.md), [`clustering_dendrogram()`](https://pinin4fjords.github.io/shinyngs/reference/clustering_dendrogram.md) | plotly / ggplot | Sample clustering dendrogram |
| [`static_boxplot()`](https://pinin4fjords.github.io/shinyngs/reference/static_boxplot.md), [`interactive_boxplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_boxplot.md), [`interactive_quartiles()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_quartiles.md) | ggplot / plotly | Per-sample abundance distributions |
| [`static_densityplot()`](https://pinin4fjords.github.io/shinyngs/reference/static_densityplot.md), [`interactive_densityplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_densityplot.md) | ggplot / plotly | Abundance density curves |
| [`interactive_topgene_boxplots()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_topgene_boxplots.md), [`static_topgene_boxplots()`](https://pinin4fjords.github.io/shinyngs/reference/static_topgene_boxplots.md) | plotly / ggplot | Faceted boxplots of top differential features |
| [`interactive_cluster_profiles()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_cluster_profiles.md) | plotly | Feature-wise cluster expression profiles |
| [`interactive_upset()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_upset.md) | plotly | Intersections of feature sets across contrasts |
| [`interactive_barcodeplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_barcodeplot.md) | plotly | Gene-set enrichment barcode over a ranking |
| [`interactive_barchart()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_barchart.md), [`interactive_count_barplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_count_barplot.md) | plotly | Bar charts and categorical counts |
| [`interactive_illumina_control_probes()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_illumina_control_probes.md) | plotly | Illumina array control-probe QC |
| [`make_color_scale()`](https://pinin4fjords.github.io/shinyngs/reference/make_color_scale.md) | character vector | A colour-blind-safe categorical palette to colour any of the above |

See the [module and panel
catalogue](https://pinin4fjords.github.io/shinyngs/articles/modules.md)
for which app panel each of these backs.

## Calling a plot from a report

A minimal example: colour a PCA scatter by a metadata variable, using
[`compile_pca_data()`](https://pinin4fjords.github.io/shinyngs/reference/compile_pca_data.md)
for the coordinates and
[`make_color_scale()`](https://pinin4fjords.github.io/shinyngs/reference/make_color_scale.md)
for a consistent palette.

``` r

library(shinyngs)

pca <- compile_pca_data(my_matrix)
groups <- my_metadata$treatment

interactive_scatterplot(
  x = pca$coords[, "PC1"],
  y = pca$coords[, "PC2"],
  colorby = groups,
  palette = make_color_scale(nlevels(factor(groups))),
  xlab = "PC1",
  ylab = "PC2"
)
```

The heatmap functions follow the same shape: pass a matrix (and,
optionally, a `sample_annotation` data frame) and get a widget back.

``` r

interactive_heatmap(
  plotmatrix = my_matrix[select_variable_genes(my_matrix, ntop = 500), ],
  sample_annotation = my_metadata["treatment"]
)
```

## Compute versus render: what `shinyngs` does and does not do

`shinyngs` is a viewer, not an analysis engine. It renders results you
have already computed. A handful of exported helpers do lightweight
preparation that the plots need, and it is fine to use them, but they
are not a statistics package:

- [`compile_pca_data()`](https://pinin4fjords.github.io/shinyngs/reference/compile_pca_data.md)
  — principal components for the scatter/scree plots
- [`select_variable_genes()`](https://pinin4fjords.github.io/shinyngs/reference/select_variable_genes.md)
  — pick the most variable features for a heatmap
- [`anova_pca_metadata()`](https://pinin4fjords.github.io/shinyngs/reference/anova_pca_metadata.md)
  — associate PCs with metadata for the association heatmap
- [`mad_score()`](https://pinin4fjords.github.io/shinyngs/reference/mad_score.md)
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

p <- interactive_scatterplot(x, y, colorby = groups)
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

[`interactive_scatterplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_scatterplot.md)’s
opt-in `colorby_menu` argument adds a dropdown *inside* the widget for
switching the colouring variable. That control is meant for
standalone/report contexts where there is no surrounding Shiny UI.
Inside a `shinyngs` app the colouring variable is chosen with a Shiny
`selectInput`, so the modules deliberately leave `colorby_menu` off to
avoid two competing colour controls on screen. If you build your own app
around these functions, pick one mechanism or the other, not both.
