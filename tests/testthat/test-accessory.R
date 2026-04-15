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
      "Intercept, genotypeWT, treatmentTreated, genotypeWT.treatmentTreated\\."
    )
  )

  unlink(yaml_file)
})
