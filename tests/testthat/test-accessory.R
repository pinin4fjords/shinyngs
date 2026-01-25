# prettifyVariablename()

test_that("prettifyVariablename works", {
  # Expect unpretty things to change
  expect_equal(prettifyVariablename("ugly_name_of_thing"), "Ugly name of thing")
  # Expect pretty things to remain unchanged
  expect_equal(prettifyVariablename("Ugly name of thing"), "Ugly name of thing")
})

# ucfirst()

test_that("ucFirst works", {
  # Expect lowercase things to change
  expect_equal(ucfirst("foo"), "Foo")
  # Expect already ucfirst to not change
  expect_equal(prettifyVariablename("FOO"), "FOO")
})

# nlines()

test_that("nlines works", {
  expect_equal(nlines("foo\nbar"), 2)
  expect_equal(nlines("foo\nbar\nfi"), 3)
})

# hiddenInput()

test_that("nlines works", {
  test_html <- "<input type='text' id='myid' value='foo' style='display: none;'>"
  expect_equal(as.character(hiddenInput("myid", "foo")), test_html)
})

# read_contrasts()

test_that("read_contrasts parses YAML correctly", {
  samples <- data.frame(
    sample = c("Sample1", "Sample7", "Sample13", "Sample19", "Sample16"),
    genotype = c("WT", "WT", "KO", "KO", "KO"),
    treatment = c("Control", "Treated", "Control", "Treated", "Control"),
    time = c(1, 1, 1, 1, 16),
    batch = c("b1", "b1", "b1", "b1", "b3"),
    stringsAsFactors = FALSE
  )

  yaml_content <- "
contrasts:
  - id: treatment_mCherry_hND6_
    comparison: [\"treatment\", \"Control\", \"Treated\"]
  - id: treatment_mCherry_hND6_batch
    comparison: [\"treatment\", \"Control\", \"Treated\"]
    blocking_factors: [\"batch\"]
  - id: treatment_plus_genotype
    formula: \"~ treatment + genotype\"
    make_contrasts_str: \"treatmentTreated\"
  - id: interaction_genotype_treatment
    formula: \"~ genotype * treatment\"
    make_contrasts_str: \"genotypeWT.treatmentTreated\"
  - id: full_model_with_interactions
    formula: \"~ genotype * treatment * time\"
    make_contrasts_str: \"genotypeWT.treatmentTreated.time\"
"

  yaml_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, yaml_file)

  contrasts <- read_contrasts(yaml_file, samples)

  # Test basic structure
  expect_true(is.data.frame(contrasts))
  expect_equal(nrow(contrasts), 5)
  expect_true(all(c("id", "variable", "reference", "target", "blocking", "formula", "make_contrasts_str") %in% colnames(contrasts)))

  # Test specific rows
  expect_equal(contrasts$variable[1], "treatment")
  expect_equal(contrasts$reference[1], "Control")
  expect_equal(contrasts$target[1], "Treated")
  expect_true(is.na(contrasts$make_contrasts_str[1]))

  expect_equal(contrasts$blocking[2], "batch")
  expect_equal(contrasts$formula[3], "~ treatment + genotype")
  expect_equal(contrasts$make_contrasts_str[4], "genotypeWT.treatmentTreated")
  expect_equal(contrasts$make_contrasts_str[5], "genotypeWT.treatmentTreated.time")

  unlink(yaml_file)
})

# read_contrasts() using only formula based contrasts

test_that("read_contrasts parses YAML correctly using only formula based contrasts", {
  samples <- data.frame(
    sample = c("Sample1", "Sample7", "Sample13", "Sample19", "Sample16"),
    genotype = c("WT", "WT", "KO", "KO", "KO"),
    treatment = c("Control", "Treated", "Control", "Treated", "Control"),
    time = c(1, 1, 1, 1, 16),
    batch = c("b1", "b1", "b1", "b1", "b3"),
    stringsAsFactors = FALSE
  )

  yaml_content <- "
contrasts:
  - id: treatment_plus_genotype
    formula: \"~ treatment + genotype\"
    make_contrasts_str: \"treatmentTreated\"
  - id: interaction_genotype_treatment
    formula: \"~ genotype * treatment\"
    make_contrasts_str: \"genotypeWT.treatmentTreated\"
  - id: full_model_with_interactions
    formula: \"~ genotype * treatment * time\"
    make_contrasts_str: \"genotypeWT.treatmentTreated.time\"
"

  yaml_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, yaml_file)

  contrasts <- read_contrasts(yaml_file, samples)

  # Test basic structure
  expect_true(is.data.frame(contrasts))
  expect_equal(nrow(contrasts), 3)
  expect_true(all(c("id", "variable", "reference", "target", "blocking", "formula", "make_contrasts_str") %in% colnames(contrasts)))

  # Test specific rows
  expect_equal(contrasts$formula[1], "~ treatment + genotype")
  expect_equal(contrasts$make_contrasts_str[2], "genotypeWT.treatmentTreated")
  expect_equal(contrasts$make_contrasts_str[3], "genotypeWT.treatmentTreated.time")

  unlink(yaml_file)
})

test_that("read_enrichment_file parses file correctly", {
  all_text <- "geneset,p value,FDR,Direction\ndummy,0.04,0.01,Up\n"
  all_file <- tempfile(pattern = "test_shinyngs", fileext = ".csv")
  all_df <- read.csv(textConnection(all_text), check.names = FALSE, row.names = 1)
  write(all_text, all_file)

  up_text <- "geneset,p value,FDR\ndummy,0.04,0.01\n"
  up_file <- tempfile(pattern = "test_shinyngs", fileext = ".csv")
  up_df <- read.csv(textConnection(up_text), check.names = FALSE, row.names = 1)
  write(up_text, up_file)
  
  down_text <- "geneset,p value,FDR\ndummy2,0.04,0.01\n"
  down_file <- tempfile(pattern = "test_shinyngs", fileext = ".csv")
  down_df <- read.csv(textConnection(down_text), check.names = FALSE, row.names = 1)
  write(down_text, down_file)
  
  up_df2 <- up_df
  up_df2$Direction <- "Up"
  down_df2 <- down_df
  down_df2$Direction <- "Down"
  combined_df <- rbind(up_df2, down_df2)

  expect_equal(read_enrichment_file(NULL), NULL)
  expect_equal(read_enrichment_file(all_file), all_df)
  expect_equal(read_enrichment_file(c("up" = up_file,"down" = down_file)),
               combined_df)
})


test_that("remove_nulls works", {
  expect_equal(remove_nulls(list(1, NULL, 2)), list(1, 2))
})

test_that("build_enrichment_path replaces variables", {
  expect_equal(
    build_enrichment_path(
      template = "./{contrast_name}/{geneset_type}/report_for_{target|reference}.csv",
      contrast_info = list(id="disease_vs_ctrl", reference="control", target="disease"),
      geneset_type = "m2.cp.v2024.1.Mm.entrez",
      direction = "up"
    ),
    "./disease_vs_ctrl/m2.cp.v2024.1.Mm.entrez/report_for_disease.csv"
  )
})

gst_gsea <- data.frame(
  "NAME" = "dummy",
  "GS<br> follow link to MSigDB" = "",
  "GS DETAILS" = "",
  "SIZE" = 0,
  "ES" = 0,
  "NES" = 0,
  "NOM p-val" = 0,
  "FDR q-val" = 0,
  "FWER p-val" = 0,
  "RANK AT MAX	LEADING EDGE" = "",
  "Direction" = "Up",
  check.names = FALSE
)

gst_roast <- data.frame(
  "NAME" = "",
  "p value" = 0.01,
  "FDR" = 0.01,
  "Direction" = "Up",
  check.names = FALSE
)

test_that("detect_enrichment_tool works", {
  expect_equal(detect_enrichment_tool(gst_gsea), "gsea")
  expect_equal(detect_enrichment_tool(gst_roast), "roast")
})

test_that("get_enrichment_mapping works", {
  mappings <- list(
    roast = list(pvalue = "p value", fdr = "FDR", direction = "Direction"),
    gsea = list(pvalue = "NOM p-val", fdr = "FDR q-val", direction = "Direction")
  )
  expect_equal(get_enrichment_mapping(gst_gsea, "gsea"), mappings$gsea)
  expect_equal(get_enrichment_mapping(gst_roast, "roast"), mappings$roast)
})

test_that("clean_enrichment_table works", {
  df <- clean_enrichment_table(gst_gsea, "gsea")
  expect_false("GS<br> follow link to MSigDB" %in% colnames(df))
})

test_that("validate_enrichment_table works", {
  validate_enrichment_table(gst_gsea, "gsea")
  validate_enrichment_table(gst_roast, "roast")
  gst_gsea_wrong <- gst_gsea
  gst_gsea_wrong[["NOM p-val"]] <- NULL
  expect_error(validate_enrichment_table(gst_gsea_wrong, "gsea"))
})
