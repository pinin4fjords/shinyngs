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

* The pkgdown site gains screenshots and diagrams: a home-page gallery, one
  screenshot per major analysis panel in the module/panel catalogue, and
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

* Every interactive plot's download button honours an app-wide PNG/SVG format
  toggle, so any plot can be exported as vector SVG for publication.
* Dropped the `reformulas` and `data.table` dependencies.
* Progress and warning logging during object construction and validation now
  goes through `message()`/`warning()` rather than `print()`, so it can be
  suppressed and captured through R's condition system.

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
