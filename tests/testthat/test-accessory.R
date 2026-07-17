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
    sample = c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5", "Sample6", "Sample7", "Sample8"),
    genotype = c("WT", "WT", "WT", "WT", "KO", "KO", "KO", "KO"),
    treatment = c("Control", "Control", "Treated", "Treated", "Control", "Control", "Treated", "Treated"),
    time = c(1, 16, 1, 16, 1, 16, 1, 16),
    batch = c("b1", "b2", "b1", "b2", "b1", "b2", "b1", "b2"),
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
    sample = c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5", "Sample6", "Sample7", "Sample8"),
    genotype = c("WT", "WT", "WT", "WT", "KO", "KO", "KO", "KO"),
    treatment = c("Control", "Control", "Treated", "Treated", "Control", "Control", "Treated", "Treated"),
    time = c(1, 16, 1, 16, 1, 16, 1, 16),
    batch = c("b1", "b2", "b1", "b2", "b1", "b2", "b1", "b2"),
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

test_that("read_contrasts reports a descriptive error for realistic invalid formula contrast strings", {
  samples <- data.frame(
    sample = c("Sample1", "Sample2", "Sample3", "Sample4", "Sample5", "Sample6", "Sample7", "Sample8"),
    genotype = c("WT", "WT", "WT", "WT", "KO", "KO", "KO", "KO"),
    treatment = c("Control", "Control", "Treated", "Treated", "Control", "Control", "Treated", "Treated"),
    stringsAsFactors = FALSE
  )

  yaml_content <- "
contrasts:
  - id: invalid_formula_contrast
    formula: \"~ genotype * treatment\"
    make_contrasts_str: \"genotypeWT.treatmenttreated\"
"

  yaml_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, yaml_file)

  expect_error(
    read_contrasts(yaml_file, samples),
    paste(
      "Contrast id 'invalid_formula_contrast' has invalid make_contrasts_str 'genotypeWT.treatmenttreated'",
      "for formula '~ genotype \\* treatment'. Available coefficient names for make_contrasts_str:",
      "X\\.Intercept\\., genotypeWT, treatmentTreated, genotypeWT\\.treatmentTreated\\."
    )
  )

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

test_that("drop_empty_gene_set_analyses keeps contrast positions but drops empty types/assays", {
  df <- data.frame(FDR = 0.01)
  gsa <- list(
    counts = list(
      # a missing middle contrast must remain NULL so positional indexing stays aligned
      go = list(c1 = df, c2 = NULL, c3 = df),
      # a gene set type with no results at all is dropped
      kegg = list(c1 = NULL, c2 = NULL, c3 = NULL)
    ),
    # an assay left with no gene set types is dropped
    other = list(go = list(c1 = NULL))
  )
  out <- drop_empty_gene_set_analyses(gsa)

  expect_equal(names(out), "counts")
  expect_equal(names(out$counts), "go")
  expect_equal(length(out$counts$go), 3)
  expect_equal(names(out$counts$go), c("c1", "c2", "c3"))
  expect_null(out$counts$go[[2]])
  expect_equal(out$counts$go[[3]], df)
})

test_that("check_gene_set_analyses_tool_consistency mirrors structure and aligns NULL contrasts", {
  df <- data.frame(FDR = 0.01)
  gsa <- list(counts = list(go = list(c1 = df, c2 = NULL, c3 = df)))
  tools <- check_gene_set_analyses_tool_consistency(gsa, list())

  expect_equal(names(tools$counts$go), c("c1", "c2", "c3"))
  expect_equal(unname(unlist(tools$counts$go)), c("auto", "auto", "auto"))

  # explicit tool choices are preserved and validated
  tools2 <- check_gene_set_analyses_tool_consistency(
    gsa, list(counts = list(go = list(c1 = "gsea", c3 = "roast")))
  )
  expect_equal(tools2$counts$go$c1, "gsea")
  expect_equal(tools2$counts$go$c3, "roast")
  expect_error(
    check_gene_set_analyses_tool_consistency(gsa, list(counts = list(go = list(c1 = "nonsense"))))
  )
})

test_that("resolve_contrast_key matches by identifier regardless of stored order", {
  # make_app_from_files style: contrast carries an id matching the entry names
  analyses <- list(neuron_no_yes = data.frame(x = 2), astrocyte_no_yes = data.frame(x = 1))
  ctr <- c(id = "astrocyte_no_yes", Variable = "Astrocyte", Group.1 = "no", Group.2 = "yes")
  # contrast is position 1, but its entry is stored second: must resolve by id, not position
  expect_equal(resolve_contrast_key(analyses, 1, ctr), "astrocyte_no_yes")

  # zhangneurons style: bare c(variable, reference, target); entries keyed var-ref-target
  z_analyses <- list(`Astrocyte-no-yes` = data.frame(x = 1), `Neuron-no-yes` = data.frame(x = 2))
  expect_equal(resolve_contrast_key(z_analyses, 2, c("Neuron", "no", "yes")), "Neuron-no-yes")

  # falls back to position when nothing matches by name
  pos_analyses <- list(data.frame(x = 1), data.frame(x = 2), data.frame(x = 3))
  expect_equal(resolve_contrast_key(pos_analyses, 3, c("Whatever", "a", "b")), 3)

  # NULL when the position is out of range and no name matches
  expect_null(resolve_contrast_key(pos_analyses, 5, c("Whatever", "a", "b")))
})

test_that("resolve_enrichment returns the cleaned table and mapping, or NULL", {
  mat <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  gsea_tab <- data.frame(
    "GS DETAILS" = "x", "NOM p-val" = 0.01, "FDR q-val" = 0.02, "Direction" = "Up",
    row.names = "SET1", check.names = FALSE, stringsAsFactors = FALSE
  )
  ese <- ExploratorySummarizedExperiment(
    assays = list(counts = mat),
    colData = data.frame(grp = c("a", "b"), batch = c("x", "y"), row.names = c("s1", "s2")),
    annotation = data.frame(gene_id = c("g1", "g2"), gene_name = c("G1", "G2"), row.names = c("g1", "g2")),
    idfield = "gene_id",
    gene_set_analyses = list(counts = list(go = list(c1 = gsea_tab, c2 = NULL)))
  )

  res <- resolve_enrichment(ese, "counts", "go", 1, c("grp", "a", "b"))
  expect_equal(res$col_map$pvalue, "NOM p-val")            # auto-detected gsea
  expect_true("SET1" %in% rownames(res$gst))
  expect_false("GS DETAILS" %in% colnames(res$gst))        # cleaned

  expect_null(resolve_enrichment(ese, "counts", "go", 2, c("grp", "x", "y")))  # NULL contrast
  expect_null(resolve_enrichment(ese, "counts", "missing", 1, c("grp", "a", "b")))  # absent type
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

# A table from an arbitrary tool with its own column names
gst_custom <- data.frame(
  "NAME" = "dummy",
  "myPval" = 0.01,
  "myFDR" = 0.02,
  "myDir" = "Up",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
custom_map <- c(pvalue = "myPval", fdr = "myFDR", direction = "myDir")

test_that("is_enrichment_mapping distinguishes mappings from tool names", {
  expect_true(is_enrichment_mapping(custom_map))
  expect_true(is_enrichment_mapping(list(pvalue = "a", fdr = "b", direction = "c")))
  expect_false(is_enrichment_mapping("gsea"))
  expect_false(is_enrichment_mapping("auto"))
  expect_false(is_enrichment_mapping(c(pvalue = "a", fdr = "b"))) # missing direction
})

test_that("get_enrichment_mapping accepts a custom column mapping for arbitrary tools", {
  expect_equal(
    get_enrichment_mapping(gst_custom, custom_map),
    list(pvalue = "myPval", fdr = "myFDR", direction = "myDir")
  )
  # named list form works too
  expect_equal(
    get_enrichment_mapping(gst_custom, list(pvalue = "myPval", fdr = "myFDR", direction = "myDir")),
    list(pvalue = "myPval", fdr = "myFDR", direction = "myDir")
  )
  # an unrecognised tool name (not a mapping) is an error
  expect_error(get_enrichment_mapping(gst_custom, "someothertool"))
})

test_that("validate_enrichment_table works with a custom mapping", {
  validate_enrichment_table(gst_custom, custom_map)
  bad <- gst_custom
  bad[["myFDR"]] <- NULL
  expect_error(validate_enrichment_table(bad, custom_map))
})

test_that("clean_enrichment_table leaves custom-tool tables untouched", {
  expect_equal(clean_enrichment_table(gst_custom, custom_map), gst_custom)
})

test_that("check_gene_set_analyses_tool_consistency accepts a custom mapping", {
  gsa <- list(counts = list(kegg = list(c1 = data.frame(x = 1))))
  tools <- check_gene_set_analyses_tool_consistency(
    gsa, list(counts = list(kegg = list(c1 = custom_map)))
  )
  expect_equal(tools$counts$kegg$c1, custom_map)
  # a mapping missing a required field is rejected
  expect_error(
    check_gene_set_analyses_tool_consistency(
      gsa, list(counts = list(kegg = list(c1 = c(pvalue = "p", fdr = "q"))))
    )
  )
})

# cond_log2_transform_matrix()

test_that("cond_log2_transform_matrix guesses reverse (unlog) status correctly", {
  raw_counts <- matrix(c(10, 2, 4, 181, 14, 12), nrow = 2, byrow = TRUE)

  # Raw counts (max well above threshold) should be left alone when guessing
  # whether to unlog, not exponentiated.
  expect_equal(
    cond_log2_transform_matrix(raw_counts, reverse = TRUE, threshold = 30),
    raw_counts
  )

  log_scale <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)

  # Small, log-scale-looking values should be unlogged when guessing.
  expect_equal(
    cond_log2_transform_matrix(log_scale, reverse = TRUE, threshold = 30),
    2^log_scale
  )
})
