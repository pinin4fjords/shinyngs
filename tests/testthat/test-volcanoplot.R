# selectVolcanoLines()

test_that("buildVolcanoTable retains legacy zero fold changes", {
  contrast_table <- data.frame(
    reference = c(1, 1),
    treatment = c(1, 2),
    `Fold change` = c(0, 2),
    `q value` = c(0.5, 0.05),
    check.names = FALSE
  )
  contrast_reactives <- list(
    selectedContrastsTables = function() list(list(contrast_table)),
    getSelectedContrasts = function() list(list(c("condition", "reference", "treatment"))),
    getFoldChangeScale = function() "linear"
  )
  session <- shiny::MockShinySession$new()
  on.exit(session$close())

  result <- shiny::withReactiveDomain(session, buildVolcanoTable(contrast_reactives))

  expect_equal(result[[1]], c(0, 1))
  expect_false(anyNA(result[[1]]))
})

make_lines <- function(fclim = -2, qvallim = 0.05) {
  data.frame(
    name = c(
      rep(paste0(abs(fclim), "-fold down"), 2),
      rep(paste0(abs(fclim), "-fold up"), 2),
      rep(paste("q <", qvallim), 2)
    ),
    x = c(-1, -1, 1, 1, -5, 5),
    y = c(0, 10, 0, 10, 2, 2)
  )
}

test_that("selectVolcanoLines keeps only the fold-down and q-value lines for '<=' with a negative limit", {
  lines <- make_lines(fclim = -2)

  result <- selectVolcanoLines(lines, fccard = "<=", fclim = -2)

  expect_equal(nrow(result), 4)
  expect_equal(rownames(result), c("1", "2", "5", "6"))
  expect_true(all(grepl("fold down|q <", result$name)))
  expect_false(any(grepl("fold up", result$name)))
})

test_that("selectVolcanoLines does not error, unlike the malformed lines[1, 2, 5, 6, ] indexing it replaced", {
  lines <- make_lines(fclim = -2)

  expect_no_error(selectVolcanoLines(lines, fccard = "<=", fclim = -2))
})

test_that("selectVolcanoLines keeps the symmetric fold-change and fold-up lines for '<=' with a positive limit", {
  lines <- make_lines(fclim = 2)

  result <- selectVolcanoLines(lines, fccard = "<=", fclim = 2)

  expect_equal(nrow(result), 4)
  expect_equal(rownames(result), c("1", "2", "3", "4"))
})

test_that("selectVolcanoLines keeps all lines when the filter is symmetric", {
  lines <- make_lines(fclim = -2)

  for (fccard in c(">= or <= -", "<= and >= -")) {
    result <- selectVolcanoLines(lines, fccard = fccard, fclim = -2)
    expect_equal(result, lines)
  }
})
