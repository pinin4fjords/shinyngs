# shinyngs (development version)

## Breaking changes

* Every exported function is now `snake_case`, and plotting functions are
  named for what they produce rather than the library that renders them.
  This removes two problems with the previous names: `plotly_*` collided in
  spirit with `plotly`'s own exports (`plotly_build()`, `plotly_json()`,
  etc.), and `colMedians()`/`colGeomMeans()` masked
  `matrixStats::colMedians()` with different output (added row names). See
  `CONTRIBUTING.md` for the naming convention going forward. No deprecated
  aliases are kept for the old names — the next release of this package
  should be a major version bump. Renamed functions:
  - Plotting: `plotly_scatterplot()` → `interactive_scatterplot()`,
    `plotly_barchart()` → `interactive_barchart()`, `plotly_boxplot()` →
    `interactive_boxplot()`, `plotly_barcodeplot()` →
    `interactive_barcodeplot()`, `plotly_cluster_profiles()` →
    `interactive_cluster_profiles()`, `plotly_clusteringDendrogram()` →
    `interactive_clustering_dendrogram()`, `plotly_count_barplot()` →
    `interactive_count_barplot()`, `plotly_densityplot()` →
    `interactive_densityplot()`, `plotly_illumina_control_probes()` →
    `interactive_illumina_control_probes()`, `plotly_pca_metadata_heatmap()`
    → `interactive_pca_metadata_heatmap()`, `plotly_pca_variance_heatmap()`
    → `interactive_pca_variance_heatmap()`, `plotly_quartiles()` →
    `interactive_quartiles()`, `plotly_screeplot()` →
    `interactive_screeplot()`, `plotly_topgene_boxplots()` →
    `interactive_topgene_boxplots()`, `plotly_upset()` →
    `interactive_upset()`, `interactiveHeatmap()` → `interactive_heatmap()`,
    `ggplot_boxplot()` → `static_boxplot()`, `ggplot_densityplot()` →
    `static_densityplot()`, `ggplot_topgene_boxplots()` →
    `static_topgene_boxplots()`, `clusteringDendrogram()` →
    `clustering_dendrogram()`, `calculateDendrogram()` →
    `calculate_dendrogram()`. `static_scatterplot()` is unchanged.
  - Matrix/stats utilities: `colMedians()` → `col_medians()`,
    `colGeomMeans()` → `col_geom_means()`, `madScore()` → `mad_score()`,
    `bootstrapMedian()` → `bootstrap_median()`, `summarizeMatrix()` →
    `summarize_matrix()`, `summarySE()` → `summary_se()`,
    `singleValidMatrix()` → `single_valid_matrix()`, `interleaveColumns()`
    → `interleave_columns()`, `simpleSplit()` → `simple_split()`,
    `calculateDist()` → `calculate_dist()`, `foldChange()` →
    `fold_change()`, `runClustering()` → `run_clustering()`,
    `selectVariableGenes()` → `select_variable_genes()`,
    `compilePCAData()` → `compile_pca_data()`.
  - String/general utilities: `ucfirst()` → `capitalize_first()`,
    `na.replace()` → `na_replace()`, `nlines()` → `count_lines()`,
    `getExtension()` → `file_extension()`, `getSeparator()` →
    `guess_separator()`, `splitStringToFixedwidthLines()` →
    `split_string_to_fixed_width_lines()`, `stringsToNamedVector()` →
    `strings_to_named_vector()`, `checkListIsSubset()` →
    `check_list_is_subset()`, `pushToList()` → `push_to_list()`,
    `prettifyGeneSetName()` → `prettify_gene_set_name()`,
    `prettifyVariablename()` → `prettify_variable_name()`, `idToLabel()` →
    `id_to_label()`, `convertIds()` → `convert_ids()`,
    `chooseGroupingVariables()` → `choose_grouping_variables()`,
    `makeColorScale()` → `make_color_scale()`, `validateOrCatch()` →
    `validate_or_catch()`.
  - Shiny UI helpers and builders: `hiddenInput()` → `hidden_input()`,
    `inlineField()` → `inline_field()`, `withHelpIcon()` →
    `with_help_icon()`, `prepareApp()` → `prepare_app()`,
    `eselistFromYAML()` → `eselist_from_yaml()`, `eselistfromConfig()` →
    `eselist_from_config()`.
  - S4 class constructors (`ExploratorySummarizedExperiment`,
    `ExploratorySummarizedExperimentList`) are unchanged — PascalCase stays
    the convention for these, matching Bioconductor practice.

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
  (`interactive_scatterplot()`, `static_scatterplot()`, `interactive_heatmap()`,
  `interactive_densityplot()`, `interactive_barcodeplot()`), the object constructors
  (`ExploratorySummarizedExperiment()`,
  `ExploratorySummarizedExperimentList()`), the file readers (`read_matrix()`
  and friends) and several compute helpers.
* Clarified that `read_differential()`/`compile_contrast_data()` and the
  `--fold_change_scale` CLI flag always return/store fold changes on a linear
  scale: whenever the scale resolves to `log2` (the common case for a
  `log2FoldChange`-named column), the values are converted from the file's
  log2 scale to linear, so callers relying on passthrough of the raw file
  values should pass `fold_change_scale = "linear"` explicitly
  ([#272](https://github.com/pinin4fjords/shinyngs/issues/272)).

## New features

* `interactive_scatterplot()` gains an opt-in `colorby_menu` dropdown for switching
  the colouring variable within a single self-contained widget, plus
  `xrange`/`yrange` arguments for pinning axis ranges (e.g. a symmetric volcano
  axis).
* New standalone, report-callable plotting functions extracted from their Shiny
  modules: `interactive_pca_metadata_heatmap()`, `interactive_upset()`,
  `interactive_cluster_profiles()`, `interactive_barchart()`,
  `interactive_illumina_control_probes()`, `interactive_count_barplot()`,
  `interactive_screeplot()` and `interactive_topgene_boxplots()` /
  `static_topgene_boxplots()`.
* A scree plot is available on the PCA panel.
* A generic category-counts plot for feature and sample metadata.

## Improvements

* `interactive_heatmap()` gains a `show_row_labels` argument for suppressing
  row labels, and now defaults `plot_height` to a height scaled to the number
  of rows: generously when labels are shown (fixing large heatmaps rendering
  squashed into a small fixed height when called outside the Shiny app), or a
  small, capped allowance when they're hidden, since a heatmap with hundreds
  of genes and no labels only needs to show the colour pattern.
* Every interactive plot's download button honours an app-wide PNG/SVG format
  toggle, so any plot can be exported as vector SVG for publication.
* Dropped the `reformulas` and `data.table` dependencies.
* Progress and warning logging during object construction and validation now
  goes through `message()`/`warning()` rather than `print()`, so it can be
  suppressed and captured through R's condition system.

## Bug fixes

* `runPCA()` and `compile_pca_data()` no longer force `prcomp()`'s
  `scale. = TRUE`. They now default to `scale = FALSE`, matching
  `DESeq2::plotPCA()`'s convention for variance-stabilised (VST/rlog) input:
  since that transform already equalises per-feature variance, further
  unit-variance scaling mostly up-weights noisy, near-constant features
  rather than revealing structure, which was found to weaken the
  association between the top PCs and experimental variables of interest in
  the "Principal components / metadata associations" view. A new
  `scale_features` argument lets callers opt back into scaling for matrices
  without a variance-stabilising transform; `exec/exploratory_plots.R`
  exposes this as `--pca_scale`/`-c`.
* `make_color_scale()` picked a named RColorBrewer palette's own 2-colour
  scheme by interpolating its full colour set down to 3 shades and
  subsetting, which could land on adjacent, low-contrast hues (e.g. `"Set1"`
  giving red and orange rather than red and blue) for 2-3 category plots
  such as `interactive_topgene_boxplots()`'s group legend.
* `upset_calculate_intersections()` always reported the highest-order
  (all-sets) intersection as 0 under the default
  `intersection_assignment_type = "upset"`, because the top intersection's
  own index range for "higher-order" members counted backwards and ended up
  excluding it from itself. This made `interactive_upset()`'s upset-style
  plots under-report the deepest intersection whenever sets genuinely
  overlapped at every level.
* `interactive_upset()`'s set-size bar chart had no axis title, was
  squeezed into a small fraction of the figure's width, and ran left-to-right
  with 0 on the left, unlike the conventional UpSet layout where the set-size
  bars grow leftward from a 0 next to the intersection grid. The chart now
  has a "Set size" axis title (with an explicit `tickangle` to keep it
  stable when the plot is rendered in an initially-hidden tab), a larger
  width allocation, and a reversed x-axis. The intersection-size chart's
  axis title was also corrected from "Intersections size" to "Intersection
  size".

## Maintenance

* Removed the unused exported function `geom_mean()`; geometric means are
  computed directly by `col_geom_means()`.
* Declared `grDevices` under `Imports` (it was already used).
* Normalised a handful of module house-style outliers: the shared
  differential-scatter helper trio is now `differentialscatterInput()` /
  `differentialscatterOutput()` / `differentialscatterLogic()` (lowercase,
  and no misleading `Server` suffix on the non-module logic helper);
  `contrasts.R` now uses `validate(need())` throughout instead of a mix with
  `req()`; and `plotlyOutput()` heights consistently use a quoted `"NNNpx"`
  string for fixed heights or a bare numeric for computed ones.
