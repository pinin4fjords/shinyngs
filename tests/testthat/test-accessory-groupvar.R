# defaultGroupvar()

test_that("defaultGroupvar prefers the explicit default_groupvar when set", {
  eselist <- methods::new("ExploratorySummarizedExperimentList", group_vars = c("condition", "batch"), default_groupvar = "batch")
  expect_equal(defaultGroupvar(eselist), "batch")
})

test_that("defaultGroupvar falls back to the first group_var when no default is set", {
  eselist <- methods::new("ExploratorySummarizedExperimentList", group_vars = c("condition", "batch"), default_groupvar = character(0))
  expect_equal(defaultGroupvar(eselist), "condition")
})

test_that("defaultGroupvar returns NULL when no grouping variables are defined", {
  eselist <- methods::new("ExploratorySummarizedExperimentList", group_vars = character(0), default_groupvar = character(0))
  expect_null(defaultGroupvar(eselist))
})
