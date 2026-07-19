# shinyngs (development version)

## Documentation

* The pkgdown site gains screenshots and diagrams: a home-page gallery, a
  screenshot for almost every analysis panel in the module/panel catalogue
  (including the platform-specific DEXSeq and Illumina array QC panels), and
  architecture diagrams showing how input data becomes an app (the data
  model article) and how modules compose into one (the developer guide).
* The single monolithic vignette has been split into a set of task-focused
  articles: getting started, the data model, building an app from files,
  a command-line interface reference, a module/panel catalogue, reusing
  components inside and outside Shiny, theming and shareable views, and a
  developer guide. The pkgdown site gains an articles menu and a changelog.
* Added runnable examples to the standalone plotting API
  (`plotly_scatterplot()`, `static_scatterplot()`, `interactiveHeatmap()`,
  `plotly_densityplot()`, `plotly_barcodeplot()`), the object constructors
  (`ExploratorySummarizedExperiment()`,
  `ExploratorySummarizedExperimentList()`), the file readers (`read_matrix()`
  and friends) and several compute helpers.

## New features

* `plotly_scatterplot()` gains an opt-in `colorby_menu` dropdown for switching
  the colouring variable within a single self-contained widget, plus
  `xrange`/`yrange` arguments for pinning axis ranges (e.g. a symmetric volcano
  axis).
* New standalone, report-callable plotting functions extracted from their Shiny
  modules: `plotly_pca_metadata_heatmap()`, `plotly_upset()`,
  `plotly_cluster_profiles()`, `plotly_barchart()`,
  `plotly_illumina_control_probes()`, `plotly_count_barplot()`,
  `plotly_screeplot()` and `plotly_topgene_boxplots()` /
  `ggplot_topgene_boxplots()`.
* A scree plot is available on the PCA panel.
* A generic category-counts plot for feature and sample metadata.

## Improvements

* Every interactive plot's download button honours an app-wide PNG/SVG format
  toggle, so any plot can be exported as vector SVG for publication.
* Dropped the `reformulas` and `data.table` dependencies.
* Progress and warning logging during object construction and validation now
  goes through `message()`/`warning()` rather than `print()`, so it can be
  suppressed and captured through R's condition system.

## Maintenance

* Removed the unused exported function `geom_mean()`; geometric means are
  computed directly by `colGeomMeans()`.
* Declared `grDevices` under `Imports` (it was already used).
* Normalised a handful of module house-style outliers: the shared
  differential-scatter helper trio is now `differentialscatterInput()` /
  `differentialscatterOutput()` / `differentialscatterLogic()` (lowercase,
  and no misleading `Server` suffix on the non-module logic helper);
  `contrasts.R` now uses `validate(need())` throughout instead of a mix with
  `req()`; and `plotlyOutput()` heights consistently use a quoted `"NNNpx"`
  string for fixed heights or a bare numeric for computed ones.
