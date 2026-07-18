# selectMaLines()

make_lines <- function(fclim = -2) {
  data.frame(
    name = c(
      rep(paste0(abs(fclim), "-fold down"), 2),
      rep(paste0(abs(fclim), "-fold up"), 2)
    ),
    x = c(-5, 5, -5, 5),
    y = c(-1, -1, 1, 1)
  )
}

test_that("selectMaLines keeps only the fold-down line for '<' with a negative limit", {
  lines <- make_lines(fclim = -2)

  result <- selectMaLines(lines, fccard = "<", fclim = -2)

  expect_equal(nrow(result), 2)
  expect_equal(rownames(result), c("1", "2"))
  expect_true(all(grepl("fold down", result$name)))
})

test_that("selectMaLines keeps only the fold-up line for '<' with a positive limit", {
  lines <- make_lines(fclim = 2)

  result <- selectMaLines(lines, fccard = "<", fclim = 2)

  expect_equal(nrow(result), 2)
  expect_equal(rownames(result), c("3", "4"))
  expect_true(all(grepl("fold up", result$name)))
})

test_that("selectMaLines keeps all lines when the filter is symmetric", {
  lines <- make_lines(fclim = -2)

  for (fccard in c("> or <-", "< and >-")) {
    result <- selectMaLines(lines, fccard = fccard, fclim = -2)
    expect_equal(result, lines)
  }
})
