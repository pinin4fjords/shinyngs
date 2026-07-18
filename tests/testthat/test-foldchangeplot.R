# selectFoldchangeLines()

make_lines <- function(fclim = -2) {
  lines <- data.frame(
    name = c(
      rep("No change", 2),
      rep(paste0(abs(fclim), "-fold down"), 2),
      rep(paste0(abs(fclim), "-fold up"), 2)
    ),
    x = c(-5, 5, -5, 5, -5, 5),
    y = c(-5, 5, -6, 4, -4, 6)
  )
  lines$name <- factor(lines$name, levels = unique(lines$name))
  lines
}

test_that("selectFoldchangeLines keeps only the no-change and fold-down lines for '<=' with a negative limit", {
  lines <- make_lines(fclim = -2)

  result <- selectFoldchangeLines(lines, fccard = "<=", fclim = -2)

  expect_equal(nrow(result), 4)
  expect_equal(rownames(result), c("1", "2", "3", "4"))
  expect_false(any(grepl("fold up", result$name)))
})

test_that("selectFoldchangeLines keeps only the no-change and fold-up lines for '<=' with a positive limit", {
  lines <- make_lines(fclim = 2)

  result <- selectFoldchangeLines(lines, fccard = "<=", fclim = 2)

  expect_equal(nrow(result), 4)
  expect_equal(rownames(result), c("1", "2", "5", "6"))
  expect_false(any(grepl("fold down", result$name)))
})

test_that("selectFoldchangeLines keeps all lines when the filter is symmetric", {
  lines <- make_lines(fclim = -2)

  for (fccard in c(">= or <= -", "<= and >= -")) {
    result <- selectFoldchangeLines(lines, fccard = fccard, fclim = -2)
    expect_equal(result, lines)
  }
})
