# Changelog

## shinyngs (development version)

### Breaking changes

- Every exported function is now `snake_case`, and plotting functions
  are named for what they produce rather than the library that renders
  them. This removes two problems with the previous names: `plotly_*`
  collided in spirit with `plotly`’s own exports (`plotly_build()`,
  `plotly_json()`, etc.), and `colMedians()`/`colGeomMeans()` masked
  [`matrixStats::colMedians()`](https://rdrr.io/pkg/matrixStats/man/rowMedians.html)
  with different output (added row names). See `CONTRIBUTING.md` for the
  naming convention going forward. No deprecated aliases are kept for
  the old names — the next release of this package should be a major
  version bump. Renamed functions:
  - Plotting: `plotly_scatterplot()` →
    [`interactive_scatterplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_scatterplot.md),
    `plotly_barchart()` →
    [`interactive_barchart()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_barchart.md),
    `plotly_boxplot()` →
    [`interactive_boxplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_boxplot.md),
    `plotly_barcodeplot()` →
    [`interactive_barcodeplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_barcodeplot.md),
    `plotly_cluster_profiles()` →
    [`interactive_cluster_profiles()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_cluster_profiles.md),
    `plotly_clusteringDendrogram()` →
    [`interactive_clustering_dendrogram()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_clustering_dendrogram.md),
    `plotly_count_barplot()` →
    [`interactive_count_barplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_count_barplot.md),
    `plotly_densityplot()` →
    [`interactive_densityplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_densityplot.md),
    `plotly_illumina_control_probes()` →
    [`interactive_illumina_control_probes()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_illumina_control_probes.md),
    `plotly_pca_metadata_heatmap()` →
    [`interactive_pca_metadata_heatmap()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_pca_metadata_heatmap.md),
    `plotly_pca_variance_heatmap()` →
    [`interactive_pca_variance_heatmap()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_pca_variance_heatmap.md),
    `plotly_quartiles()` →
    [`interactive_quartiles()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_quartiles.md),
    `plotly_screeplot()` →
    [`interactive_screeplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_screeplot.md),
    `plotly_topgene_boxplots()` →
    [`interactive_topgene_boxplots()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_topgene_boxplots.md),
    `plotly_upset()` →
    [`interactive_upset()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_upset.md),
    `interactiveHeatmap()` →
    [`interactive_heatmap()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_heatmap.md),
    `ggplot_boxplot()` →
    [`static_boxplot()`](https://pinin4fjords.github.io/shinyngs/reference/static_boxplot.md),
    `ggplot_densityplot()` →
    [`static_densityplot()`](https://pinin4fjords.github.io/shinyngs/reference/static_densityplot.md),
    `ggplot_topgene_boxplots()` →
    [`static_topgene_boxplots()`](https://pinin4fjords.github.io/shinyngs/reference/static_topgene_boxplots.md),
    `clusteringDendrogram()` →
    [`clustering_dendrogram()`](https://pinin4fjords.github.io/shinyngs/reference/clustering_dendrogram.md),
    `calculateDendrogram()` →
    [`calculate_dendrogram()`](https://pinin4fjords.github.io/shinyngs/reference/calculate_dendrogram.md).
    [`static_scatterplot()`](https://pinin4fjords.github.io/shinyngs/reference/static_scatterplot.md)
    is unchanged.
  - Matrix/stats utilities: `colMedians()` →
    [`col_medians()`](https://pinin4fjords.github.io/shinyngs/reference/col_medians.md),
    `colGeomMeans()` →
    [`col_geom_means()`](https://pinin4fjords.github.io/shinyngs/reference/col_geom_means.md),
    `madScore()` →
    [`mad_score()`](https://pinin4fjords.github.io/shinyngs/reference/mad_score.md),
    `bootstrapMedian()` →
    [`bootstrap_median()`](https://pinin4fjords.github.io/shinyngs/reference/bootstrap_median.md),
    `summarizeMatrix()` →
    [`summarize_matrix()`](https://pinin4fjords.github.io/shinyngs/reference/summarize_matrix.md),
    `summarySE()` →
    [`summary_se()`](https://pinin4fjords.github.io/shinyngs/reference/summary_se.md),
    `singleValidMatrix()` →
    [`single_valid_matrix()`](https://pinin4fjords.github.io/shinyngs/reference/single_valid_matrix.md),
    `interleaveColumns()` →
    [`interleave_columns()`](https://pinin4fjords.github.io/shinyngs/reference/interleave_columns.md),
    `simpleSplit()` →
    [`simple_split()`](https://pinin4fjords.github.io/shinyngs/reference/simple_split.md),
    `calculateDist()` →
    [`calculate_dist()`](https://pinin4fjords.github.io/shinyngs/reference/calculate_dist.md),
    `foldChange()` →
    [`fold_change()`](https://pinin4fjords.github.io/shinyngs/reference/fold_change.md),
    `runClustering()` →
    [`run_clustering()`](https://pinin4fjords.github.io/shinyngs/reference/run_clustering.md),
    `selectVariableGenes()` →
    [`select_variable_genes()`](https://pinin4fjords.github.io/shinyngs/reference/select_variable_genes.md),
    `compilePCAData()` →
    [`compile_pca_data()`](https://pinin4fjords.github.io/shinyngs/reference/compile_pca_data.md).
  - String/general utilities: `ucfirst()` →
    [`capitalize_first()`](https://pinin4fjords.github.io/shinyngs/reference/capitalize_first.md),
    `na.replace()` →
    [`na_replace()`](https://pinin4fjords.github.io/shinyngs/reference/na_replace.md),
    `nlines()` →
    [`count_lines()`](https://pinin4fjords.github.io/shinyngs/reference/count_lines.md),
    `getExtension()` →
    [`file_extension()`](https://pinin4fjords.github.io/shinyngs/reference/file_extension.md),
    `getSeparator()` →
    [`guess_separator()`](https://pinin4fjords.github.io/shinyngs/reference/guess_separator.md),
    `splitStringToFixedwidthLines()` →
    [`split_string_to_fixed_width_lines()`](https://pinin4fjords.github.io/shinyngs/reference/split_string_to_fixed_width_lines.md),
    `stringsToNamedVector()` →
    [`strings_to_named_vector()`](https://pinin4fjords.github.io/shinyngs/reference/strings_to_named_vector.md),
    `checkListIsSubset()` →
    [`check_list_is_subset()`](https://pinin4fjords.github.io/shinyngs/reference/check_list_is_subset.md),
    `pushToList()` →
    [`push_to_list()`](https://pinin4fjords.github.io/shinyngs/reference/push_to_list.md),
    `prettifyGeneSetName()` →
    [`prettify_gene_set_name()`](https://pinin4fjords.github.io/shinyngs/reference/prettify_gene_set_name.md),
    `prettifyVariablename()` →
    [`prettify_variable_name()`](https://pinin4fjords.github.io/shinyngs/reference/prettify_variable_name.md),
    `idToLabel()` →
    [`id_to_label()`](https://pinin4fjords.github.io/shinyngs/reference/id_to_label.md),
    `convertIds()` →
    [`convert_ids()`](https://pinin4fjords.github.io/shinyngs/reference/convert_ids.md),
    `chooseGroupingVariables()` →
    [`choose_grouping_variables()`](https://pinin4fjords.github.io/shinyngs/reference/choose_grouping_variables.md),
    `makeColorScale()` →
    [`make_color_scale()`](https://pinin4fjords.github.io/shinyngs/reference/make_color_scale.md),
    `validateOrCatch()` →
    [`validate_or_catch()`](https://pinin4fjords.github.io/shinyngs/reference/validate_or_catch.md).
  - Shiny UI helpers and builders: `hiddenInput()` →
    [`hidden_input()`](https://pinin4fjords.github.io/shinyngs/reference/hidden_input.md),
    `inlineField()` →
    [`inline_field()`](https://pinin4fjords.github.io/shinyngs/reference/inline_field.md),
    `withHelpIcon()` →
    [`with_help_icon()`](https://pinin4fjords.github.io/shinyngs/reference/with_help_icon.md),
    `prepareApp()` →
    [`prepare_app()`](https://pinin4fjords.github.io/shinyngs/reference/prepare_app.md),
    `eselistFromYAML()` →
    [`eselist_from_yaml()`](https://pinin4fjords.github.io/shinyngs/reference/eselist_from_yaml.md),
    `eselistfromConfig()` →
    [`eselist_from_config()`](https://pinin4fjords.github.io/shinyngs/reference/eselist_from_config.md).
  - S4 class constructors (`ExploratorySummarizedExperiment`,
    `ExploratorySummarizedExperimentList`) are unchanged — PascalCase
    stays the convention for these, matching Bioconductor practice.

### Documentation

- The pkgdown site gains screenshots and diagrams: a home-page gallery,
  a screenshot for almost every analysis panel in the module/panel
  catalogue (including the platform-specific DEXSeq and Illumina array
  QC panels), and architecture diagrams showing how input data becomes
  an app (the data model article) and how modules compose into one (the
  developer guide).
- The single monolithic vignette has been split into a set of
  task-focused articles: getting started, the data model, building an
  app from files, a command-line interface reference, a module/panel
  catalogue, reusing components inside and outside Shiny, theming and
  shareable views, and a developer guide. The pkgdown site gains an
  articles menu and a changelog.
- Added runnable examples to the standalone plotting API
  ([`interactive_scatterplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_scatterplot.md),
  [`static_scatterplot()`](https://pinin4fjords.github.io/shinyngs/reference/static_scatterplot.md),
  [`interactive_heatmap()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_heatmap.md),
  [`interactive_densityplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_densityplot.md),
  [`interactive_barcodeplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_barcodeplot.md)),
  the object constructors
  ([`ExploratorySummarizedExperiment()`](https://pinin4fjords.github.io/shinyngs/reference/ExploratorySummarizedExperiment.md),
  [`ExploratorySummarizedExperimentList()`](https://pinin4fjords.github.io/shinyngs/reference/ExploratorySummarizedExperimentList.md)),
  the file readers
  ([`read_matrix()`](https://pinin4fjords.github.io/shinyngs/reference/read_matrix.md)
  and friends) and several compute helpers.
- Clarified that
  [`read_differential()`](https://pinin4fjords.github.io/shinyngs/reference/read_differential.md)/[`compile_contrast_data()`](https://pinin4fjords.github.io/shinyngs/reference/compile_contrast_data.md)
  and the `--fold_change_scale` CLI flag always return/store fold
  changes on a linear scale: whenever the scale resolves to `log2` (the
  common case for a `log2FoldChange`-named column), the values are
  converted from the file’s log2 scale to linear, so callers relying on
  passthrough of the raw file values should pass
  `fold_change_scale = "linear"` explicitly
  ([\#272](https://github.com/pinin4fjords/shinyngs/issues/272)).

### New features

- [`interactive_scatterplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_scatterplot.md)
  gains an opt-in `colorby_menu` dropdown for switching the colouring
  variable within a single self-contained widget, plus `xrange`/`yrange`
  arguments for pinning axis ranges (e.g. a symmetric volcano axis).
- New standalone, report-callable plotting functions extracted from
  their Shiny modules:
  [`interactive_pca_metadata_heatmap()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_pca_metadata_heatmap.md),
  [`interactive_upset()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_upset.md),
  [`interactive_cluster_profiles()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_cluster_profiles.md),
  [`interactive_barchart()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_barchart.md),
  [`interactive_illumina_control_probes()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_illumina_control_probes.md),
  [`interactive_count_barplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_count_barplot.md),
  [`interactive_screeplot()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_screeplot.md)
  and
  [`interactive_topgene_boxplots()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_topgene_boxplots.md)
  /
  [`static_topgene_boxplots()`](https://pinin4fjords.github.io/shinyngs/reference/static_topgene_boxplots.md).
- A scree plot is available on the PCA panel.
- A generic category-counts plot for feature and sample metadata.

### Improvements

- Every interactive plot’s download button honours an app-wide PNG/SVG
  format toggle, so any plot can be exported as vector SVG for
  publication.
- Dropped the `reformulas` and `data.table` dependencies.
- Progress and warning logging during object construction and validation
  now goes through
  [`message()`](https://rdrr.io/r/base/message.html)/[`warning()`](https://rdrr.io/r/base/warning.html)
  rather than [`print()`](https://rdrr.io/r/base/print.html), so it can
  be suppressed and captured through R’s condition system.

### Bug fixes

- [`runPCA()`](https://pinin4fjords.github.io/shinyngs/reference/runPCA.md)
  and
  [`compile_pca_data()`](https://pinin4fjords.github.io/shinyngs/reference/compile_pca_data.md)
  no longer force [`prcomp()`](https://rdrr.io/r/stats/prcomp.html)’s
  `scale. = TRUE`. They now default to `scale = FALSE`, matching
  [`DESeq2::plotPCA()`](https://rdrr.io/pkg/BiocGenerics/man/plotPCA.html)’s
  convention for variance-stabilised (VST/rlog) input: since that
  transform already equalises per-feature variance, further
  unit-variance scaling mostly up-weights noisy, near-constant features
  rather than revealing structure, which was found to weaken the
  association between the top PCs and experimental variables of interest
  in the “Principal components / metadata associations” view. A new
  `scale_features` argument lets callers opt back into scaling for
  matrices without a variance-stabilising transform;
  `exec/exploratory_plots.R` exposes this as `--pca_scale`/`-c`.
- [`make_color_scale()`](https://pinin4fjords.github.io/shinyngs/reference/make_color_scale.md)
  picked a named RColorBrewer palette’s own 2-colour scheme by
  interpolating its full colour set down to 3 shades and subsetting,
  which could land on adjacent, low-contrast hues (e.g. `"Set1"` giving
  red and orange rather than red and blue) for 2-3 category plots such
  as
  [`interactive_topgene_boxplots()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_topgene_boxplots.md)’s
  group legend.
- [`upset_calculate_intersections()`](https://pinin4fjords.github.io/shinyngs/reference/upset_calculate_intersections.md)
  always reported the highest-order (all-sets) intersection as 0 under
  the default `intersection_assignment_type = "upset"`, because the top
  intersection’s own index range for “higher-order” members counted
  backwards and ended up excluding it from itself. This made
  [`interactive_upset()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_upset.md)’s
  upset-style plots under-report the deepest intersection whenever sets
  genuinely overlapped at every level.
- [`interactive_upset()`](https://pinin4fjords.github.io/shinyngs/reference/interactive_upset.md)’s
  set-size bar chart had no axis title, was squeezed into a small
  fraction of the figure’s width, and ran left-to-right with 0 on the
  left, unlike the conventional UpSet layout where the set-size bars
  grow leftward from a 0 next to the intersection grid. The chart now
  has a “Set size” axis title (with an explicit `tickangle` to keep it
  stable when the plot is rendered in an initially-hidden tab), a larger
  width allocation, and a reversed x-axis. The intersection-size chart’s
  axis title was also corrected from “Intersections size” to
  “Intersection size”.

### Maintenance

- Removed the unused exported function `geom_mean()`; geometric means
  are computed directly by
  [`col_geom_means()`](https://pinin4fjords.github.io/shinyngs/reference/col_geom_means.md).
- Declared `grDevices` under `Imports` (it was already used).
- Normalised a handful of module house-style outliers: the shared
  differential-scatter helper trio is now
  [`differentialscatterInput()`](https://pinin4fjords.github.io/shinyngs/reference/differentialscatterInput.md)
  /
  [`differentialscatterOutput()`](https://pinin4fjords.github.io/shinyngs/reference/differentialscatterOutput.md)
  /
  [`differentialscatterLogic()`](https://pinin4fjords.github.io/shinyngs/reference/differentialscatterLogic.md)
  (lowercase, and no misleading `Server` suffix on the non-module logic
  helper); `contrasts.R` now uses `validate(need())` throughout instead
  of a mix with `req()`; and `plotlyOutput()` heights consistently use a
  quoted `"NNNpx"` string for fixed heights or a bare numeric for
  computed ones.
