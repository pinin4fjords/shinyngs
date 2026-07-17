# exec/validate_fom_components.R

test_that("validate_fom_components.R runs against fixtures and rewrites normalized outputs", {
  skip_on_cran()

  outdir <- withr::local_tempdir()

  result <- run_exec_script("validate_fom_components.R", c(
    "--sample_metadata", exec_smoke_fixture("SRP254919.samplesheet.csv"),
    "--feature_metadata", exec_smoke_fixture("SRP254919.gene_meta.tsv"),
    "--assay_files", exec_smoke_fixture("SRP254919.salmon.merged.gene_counts.top1000cov.tsv"),
    "--contrasts_file", exec_smoke_fixture("SRP254919.contrasts.csv"),
    "--output_directory", outdir
  ))

  expect_exec_success(result)

  expect_true(file.exists(file.path(outdir, "SRP254919.samplesheet.sample_metadata.tsv")))
  expect_true(file.exists(file.path(outdir, "SRP254919.gene_meta.feature_metadata.tsv")))
  expect_true(file.exists(file.path(outdir, "SRP254919.contrasts.contrasts_file.tsv")))
  expect_true(file.exists(file.path(outdir, "SRP254919.salmon.merged.gene_counts.top1000cov.assay.tsv")))
})

# exec/exploratory_plots.R

test_that("exploratory_plots.R runs against fixtures and writes plot PNGs", {
  skip_on_cran()

  outdir <- withr::local_tempdir()

  result <- run_exec_script("exploratory_plots.R", c(
    "--sample_metadata", exec_smoke_fixture("SRP254919.samplesheet.csv"),
    "--feature_metadata", exec_smoke_fixture("SRP254919.gene_meta.tsv"),
    "--assay_files", exec_smoke_fixture("SRP254919.salmon.merged.gene_counts.top1000cov.tsv"),
    "--contrast_variable", "treatment",
    "--outdir", outdir
  ))

  expect_exec_success(result)

  expected_pngs <- c("boxplot.png", "density.png", "pca2d.png", "pca3d.png", "sample_dendrogram.png", "mad_correlation.png")
  for (png_file in expected_pngs) {
    png_path <- file.path(outdir, "png", png_file)
    expect_true(file.exists(png_path), info = png_path)
    expect_gt(file.size(png_path), 0)
  }
})

# exec/differential_plots.R

test_that("differential_plots.R runs against fixtures and writes a volcano plot", {
  skip_on_cran()

  outdir <- withr::local_tempdir()

  result <- run_exec_script("differential_plots.R", c(
    "--differential_file", exec_smoke_fixture("SRP254919.salmon.merged.deseq2.results.tsv"),
    "--feature_metadata", exec_smoke_fixture("SRP254919.gene_meta.tsv"),
    "--reference_level", "mCherry",
    "--treatment_level", "hND6",
    "--outdir", outdir
  ))

  expect_exec_success(result)

  volcano_path <- file.path(outdir, "png", "volcano.png")
  expect_true(file.exists(volcano_path))
  expect_gt(file.size(volcano_path), 0)
})

# exec/make_app_from_files.R

test_that("make_app_from_files.R runs against fixtures and writes a loadable app bundle", {
  skip_on_cran()

  outdir <- withr::local_tempdir()

  # The contrasts fixture has two rows; --differential_results expects one
  # file per row, so a second copy stands in for the second contrast.
  second_differential_file <- withr::local_tempfile(fileext = ".tsv")
  file.copy(exec_smoke_fixture("SRP254919.salmon.merged.deseq2.results.tsv"), second_differential_file)

  result <- run_exec_script("make_app_from_files.R", c(
    "--title", "Smoke test app",
    "--author", "Smoke test",
    "--sample_metadata", exec_smoke_fixture("SRP254919.samplesheet.csv"),
    "--feature_metadata", exec_smoke_fixture("SRP254919.gene_meta.tsv"),
    "--assay_files", exec_smoke_fixture("SRP254919.salmon.merged.gene_counts.top1000cov.tsv"),
    "--contrast_file", exec_smoke_fixture("SRP254919.contrasts.csv"),
    "--contrast_stats_assay", "1",
    "--differential_results", paste(exec_smoke_fixture("SRP254919.salmon.merged.deseq2.results.tsv"), second_differential_file, sep = ","),
    "--output_directory", outdir
  ))

  expect_exec_success(result)

  rds_path <- file.path(outdir, "data.rds")
  expect_true(file.exists(rds_path))
  expect_true(file.exists(file.path(outdir, "app.R")))

  eselist <- readRDS(rds_path)
  expect_s4_class(eselist, "ExploratorySummarizedExperimentList")
  expect_length(eselist, 1)
})
