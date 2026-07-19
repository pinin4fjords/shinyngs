# plotly_illumina_control_probes()

make_control_fixture <- function() {
  control_annotation <- data.frame(
    Array_Address_Id = paste0("probe", 1:9),
    Reporter_Group_id = c(
      "phage_lambda_genome:low", "phage_lambda_genome:med", "phage_lambda_genome:high",
      "phage_lambda_genome:pm", "phage_lambda_genome:mm2", "permuted_negative",
      "phage_lambda_genome", "thrB", "housekeeping"
    )
  )
  controls <- matrix(seq_len(27),
    nrow = 9, dimnames = list(control_annotation$Array_Address_Id, paste0("sample", 1:3))
  )
  list(control_annotation = control_annotation, controls = controls)
}

test_that("plotly_illumina_control_probes draws one line trace per QC group, averaged across matching probes", {
  fixture <- make_control_fixture()

  built <- plotly::plotly_build(plotly_illumina_control_probes(fixture$control_annotation, fixture$controls))

  trace_names <- vapply(built$x$data, function(t) as.character(t$name), character(1))
  expect_setequal(
    trace_names,
    c("cy3_low", "cy3_med", "cy3_high", "low_stringency_pm", "low_stringency_mm", "negative", "biotin", "labeling", "housekeeping")
  )

  cy3_low <- built$x$data[[which(trace_names == "cy3_low")]]
  expect_equal(as.numeric(cy3_low$y), as.numeric(fixture$controls["probe1", ]))
})

test_that("plotly_illumina_control_probes averages multiple probes matching the same QC group", {
  fixture <- make_control_fixture()
  # a second probe also tagged "housekeeping" (comma-separated group membership)
  fixture$control_annotation$Reporter_Group_id[fixture$control_annotation$Array_Address_Id == "probe1"] <- "housekeeping,phage_lambda_genome:low"

  built <- plotly::plotly_build(plotly_illumina_control_probes(fixture$control_annotation, fixture$controls))

  trace_names <- vapply(built$x$data, function(t) as.character(t$name), character(1))
  housekeeping <- built$x$data[[which(trace_names == "housekeeping")]]

  expect_equal(as.numeric(housekeeping$y), unname(colMeans(fixture$controls[c("probe1", "probe9"), ])))
})

test_that("plotly_illumina_control_probes respects a custom sample display order", {
  fixture <- make_control_fixture()

  built <- plotly::plotly_build(plotly_illumina_control_probes(fixture$control_annotation, fixture$controls, sample_order = c("sample3", "sample1", "sample2")))

  expect_equal(built$x$layout$xaxis$categoryarray, c("sample3", "sample1", "sample2"))
})
