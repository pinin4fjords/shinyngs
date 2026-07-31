gene_contrast_profile_table <- function(include_q = TRUE) {
  table <- data.frame(
    Variable = rep("condition", 3),
    `Condition 1` = rep("control", 3),
    `Condition 2` = c("treated_a", "treated_b", "treated_c"),
    `Fold change` = c(4, -2, 1),
    `p value` = c(0.001, 0.02, 0.8),
    check.names = FALSE
  )
  if (include_q) {
    table[["q value"]] <- c(0.01, 0.2, NA)
  }
  table
}

test_that("interactive_gene_contrast_profile plots every finite contrast", {
  built <- plotly::plotly_build(interactive_gene_contrast_profile(gene_contrast_profile_table()))

  expect_equal(sort(unlist(lapply(built$x$data, function(trace) as.numeric(trace$x)))), c(-1, 0, 2))
  expect_equal(sort(unlist(lapply(built$x$data, function(trace) as.character(trace$y)))), sort(c(
    "Condition: treated_a vs control",
    "Condition: treated_b vs control",
    "Condition: treated_c vs control"
  )))
  expect_length(built$x$layout$shapes, 1)
})

test_that("interactive_gene_contrast_profile preserves zero fold changes", {
  table <- gene_contrast_profile_table()[1, ]
  table[["Fold change"]] <- 0
  built <- plotly::plotly_build(interactive_gene_contrast_profile(table))

  expect_equal(as.numeric(built$x$data[[1]]$x), 0)
})

test_that("interactive_gene_contrast_profile encodes q-value status in marker symbols", {
  built <- plotly::plotly_build(interactive_gene_contrast_profile(gene_contrast_profile_table()))
  symbols <- unlist(lapply(built$x$data, function(trace) trace$marker$symbol))

  expect_setequal(symbols, c("circle", "circle-open", "x"))
})

test_that("interactive_gene_contrast_profile works without q values", {
  built <- plotly::plotly_build(interactive_gene_contrast_profile(gene_contrast_profile_table(include_q = FALSE)))
  symbols <- unlist(lapply(built$x$data, function(trace) trace$marker$symbol))

  expect_true(all(symbols == "x"))
})

test_that("interactive_gene_contrast_profile uses canonical contrast names", {
  table <- gene_contrast_profile_table()[1:2, ]
  table[["Condition 2"]] <- "treated"
  table$Contrast <- c(
    "Condition: treated vs control (batch:a)",
    "Condition: treated vs control (batch:b)"
  )
  built <- plotly::plotly_build(interactive_gene_contrast_profile(table))

  expect_setequal(
    unlist(lapply(built$x$data, function(trace) as.character(trace$y))),
    table$Contrast
  )
})

test_that("interactive_gene_contrast_profile reports missing required columns", {
  expect_error(
    interactive_gene_contrast_profile(data.frame(value = 2)),
    "missing required column: Fold change"
  )
})
