# foldChange()

test_that("foldChange returns 1 where the two vectors are equal", {
  expect_equal(foldChange(c(2, 2, 2), c(2, 2, 2)), c(1, 1, 1))
})

test_that("foldChange returns a plain ratio when vec2 exceeds vec1", {
  expect_equal(foldChange(c(2, 2, 2), c(2, 4, 8)), c(1, 2, 4))
})

test_that("foldChange returns a negative reciprocal when vec2 is smaller than vec1", {
  expect_equal(foldChange(c(4, 8), c(2, 2)), c(-2, -4))
})

test_that("foldChange handles a mix of increases, decreases and no change", {
  vec1 <- c(10, 10, 10)
  vec2 <- c(10, 20, 5)

  expect_equal(foldChange(vec1, vec2), c(1, 2, -2))
})
