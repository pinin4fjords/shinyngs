# summarize_matrix()

test_that("summarize_matrix averages columns by treatment group", {
  m <- matrix(1:8, nrow = 2, dimnames = list(c("g1", "g2"), paste0("s", 1:4)))
  treatment <- c("A", "A", "B", "B")

  summarized <- summarize_matrix(m, treatment)

  expect_equal(colnames(summarized), c("A", "B"))
  expect_equal(summarized["g1", "A"], mean(m["g1", 1:2]))
  expect_equal(summarized["g1", "B"], mean(m["g1", 3:4]))
})

test_that("summarize_matrix accepts a character vector and coerces it to a factor", {
  m <- matrix(1:8, nrow = 2, dimnames = list(c("g1", "g2"), paste0("s", 1:4)))
  by_factor <- summarize_matrix(m, factor(c("A", "A", "B", "B")))
  by_character <- summarize_matrix(m, c("A", "A", "B", "B"))

  expect_equal(by_factor, by_character)
})

test_that("summarize_matrix replaces NA treatment levels with a group of their own", {
  m <- matrix(1:8, nrow = 2, dimnames = list(c("g1", "g2"), paste0("s", 1:4)))
  treatment <- c("A", NA, "B", "B")

  summarized <- summarize_matrix(m, treatment)

  expect_true("NA" %in% colnames(summarized))
  expect_equal(summarized["g1", "NA"], m["g1", 2])
})

test_that("summarize_matrix supports col_geom_means and col_medians as summary functions", {
  m <- matrix(c(1, 2, 4, 8), nrow = 1, dimnames = list("g1", paste0("s", 1:4)))
  treatment <- c("A", "A", "B", "B")

  geom <- summarize_matrix(m, treatment, summaryFunc = "col_geom_means")
  med <- summarize_matrix(m, treatment, summaryFunc = "col_medians")

  expect_equal(geom["g1", "A"], sqrt(1 * 2))
  expect_equal(med["g1", "B"], median(c(4, 8)))
})

# col_geom_means()

test_that("col_geom_means computes the geometric mean of each column", {
  x <- matrix(c(1, 4, 2, 8), nrow = 2, dimnames = list(NULL, c("a", "b")))

  result <- col_geom_means(x)

  expect_equal(unname(result), c(sqrt(1 * 4), sqrt(2 * 8)))
  expect_equal(names(result), c("a", "b"))
})

test_that("col_geom_means ignores non-positive and missing values", {
  x <- matrix(c(1, 4, NA, 0, -1, 9), nrow = 3, dimnames = list(NULL, c("a", "b")))

  result <- col_geom_means(x)

  # column a: only 1 and 4 are positive
  expect_equal(unname(result["a"]), exp((log(1) + log(4)) / 3))
})

# col_medians()

test_that("col_medians computes the median of each column", {
  x <- matrix(c(1, 2, 3, 10, 20, 30), nrow = 3, dimnames = list(NULL, c("a", "b")))

  result <- col_medians(x)

  expect_equal(unname(result), c(2, 20))
  expect_equal(names(result), c("a", "b"))
})
