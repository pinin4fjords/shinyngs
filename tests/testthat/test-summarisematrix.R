# summarizeMatrix()

test_that("summarizeMatrix averages columns by treatment group", {
  m <- matrix(1:8, nrow = 2, dimnames = list(c("g1", "g2"), paste0("s", 1:4)))
  treatment <- c("A", "A", "B", "B")

  summarized <- summarizeMatrix(m, treatment)

  expect_equal(colnames(summarized), c("A", "B"))
  expect_equal(summarized["g1", "A"], mean(m["g1", 1:2]))
  expect_equal(summarized["g1", "B"], mean(m["g1", 3:4]))
})

test_that("summarizeMatrix accepts a character vector and coerces it to a factor", {
  m <- matrix(1:8, nrow = 2, dimnames = list(c("g1", "g2"), paste0("s", 1:4)))
  by_factor <- summarizeMatrix(m, factor(c("A", "A", "B", "B")))
  by_character <- summarizeMatrix(m, c("A", "A", "B", "B"))

  expect_equal(by_factor, by_character)
})

test_that("summarizeMatrix replaces NA treatment levels with a group of their own", {
  m <- matrix(1:8, nrow = 2, dimnames = list(c("g1", "g2"), paste0("s", 1:4)))
  treatment <- c("A", NA, "B", "B")

  summarized <- summarizeMatrix(m, treatment)

  expect_true("NA" %in% colnames(summarized))
  expect_equal(summarized["g1", "NA"], m["g1", 2])
})

test_that("summarizeMatrix supports colGeomMeans and colMedians as summary functions", {
  m <- matrix(c(1, 2, 4, 8), nrow = 1, dimnames = list("g1", paste0("s", 1:4)))
  treatment <- c("A", "A", "B", "B")

  geom <- summarizeMatrix(m, treatment, summaryFunc = "colGeomMeans")
  med <- summarizeMatrix(m, treatment, summaryFunc = "colMedians")

  expect_equal(geom["g1", "A"], sqrt(1 * 2))
  expect_equal(med["g1", "B"], median(c(4, 8)))
})

# colGeomMeans()

test_that("colGeomMeans computes the geometric mean of each column", {
  x <- matrix(c(1, 4, 2, 8), nrow = 2, dimnames = list(NULL, c("a", "b")))

  result <- colGeomMeans(x)

  expect_equal(unname(result), c(sqrt(1 * 4), sqrt(2 * 8)))
  expect_equal(names(result), c("a", "b"))
})

test_that("colGeomMeans ignores non-positive and missing values", {
  x <- matrix(c(1, 4, NA, 0, -1, 9), nrow = 3, dimnames = list(NULL, c("a", "b")))

  result <- colGeomMeans(x)

  # column a: only 1 and 4 are positive
  expect_equal(unname(result["a"]), exp((log(1) + log(4)) / 3))
})

# colMedians()

test_that("colMedians computes the median of each column", {
  x <- matrix(c(1, 2, 3, 10, 20, 30), nrow = 3, dimnames = list(NULL, c("a", "b")))

  result <- colMedians(x)

  expect_equal(unname(result), c(2, 20))
  expect_equal(names(result), c("a", "b"))
})
