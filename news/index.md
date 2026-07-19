# Changelog

## shinyngs (development version)

### Documentation

- The single monolithic vignette has been split into a set of
  task-focused articles: getting started, the data model, building an
  app from files, a command-line interface reference, a module/panel
  catalogue, reusing components inside and outside Shiny, theming and
  shareable views, and a developer guide. The pkgdown site gains an
  articles menu and a changelog.
- Added runnable examples to the standalone plotting API
  ([`plotly_scatterplot()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_scatterplot.md),
  [`static_scatterplot()`](https://pinin4fjords.github.io/shinyngs/reference/static_scatterplot.md),
  [`interactiveHeatmap()`](https://pinin4fjords.github.io/shinyngs/reference/interactiveHeatmap.md),
  [`plotly_densityplot()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_densityplot.md),
  [`plotly_barcodeplot()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_barcodeplot.md)),
  the object constructors
  ([`ExploratorySummarizedExperiment()`](https://pinin4fjords.github.io/shinyngs/reference/ExploratorySummarizedExperiment.md),
  [`ExploratorySummarizedExperimentList()`](https://pinin4fjords.github.io/shinyngs/reference/ExploratorySummarizedExperimentList.md)),
  the file readers
  ([`read_matrix()`](https://pinin4fjords.github.io/shinyngs/reference/read_matrix.md)
  and friends) and several compute helpers.

### New features

- [`plotly_scatterplot()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_scatterplot.md)
  gains an opt-in `colorby_menu` dropdown for switching the colouring
  variable within a single self-contained widget, plus `xrange`/`yrange`
  arguments for pinning axis ranges (e.g. a symmetric volcano axis).
- New standalone, report-callable plotting functions extracted from
  their Shiny modules:
  [`plotly_pca_metadata_heatmap()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_pca_metadata_heatmap.md),
  [`plotly_upset()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_upset.md),
  [`plotly_cluster_profiles()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_cluster_profiles.md),
  [`plotly_barchart()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_barchart.md),
  [`plotly_illumina_control_probes()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_illumina_control_probes.md),
  [`plotly_count_barplot()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_count_barplot.md),
  [`plotly_screeplot()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_screeplot.md)
  and
  [`plotly_topgene_boxplots()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_topgene_boxplots.md)
  /
  [`ggplot_topgene_boxplots()`](https://pinin4fjords.github.io/shinyngs/reference/ggplot_topgene_boxplots.md).
- A scree plot is available on the PCA panel.
- A generic category-counts plot for feature and sample metadata.

### Improvements

- Every interactive plot’s download button honours an app-wide PNG/SVG
  format toggle, so any plot can be exported as vector SVG for
  publication.
- Dropped the `reformulas` and `data.table` dependencies.
