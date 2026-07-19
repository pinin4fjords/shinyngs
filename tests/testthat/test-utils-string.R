# simpleSplit()

test_that("simpleSplit splits a delimited string into a character vector", {
  expect_equal(simpleSplit("a,b,c"), c("a", "b", "c"))
})

test_that("simpleSplit supports a custom separator", {
  expect_equal(simpleSplit("a;b;c", sep = ";"), c("a", "b", "c"))
})

test_that("simpleSplit returns NA unchanged", {
  expect_equal(simpleSplit(NA), NA)
})

# getExtension()

test_that("getExtension extracts a lower-cased file extension", {
  expect_equal(getExtension("data.CSV"), "csv")
  expect_equal(getExtension("/some/path/data.tsv"), "tsv")
})

test_that("getExtension uses the last extension of a multi-dot filename", {
  expect_equal(getExtension("archive.tar.gz"), "gz")
})

# getSeparator()

test_that("getSeparator infers tab for tsv/txt and comma for csv", {
  expect_equal(getSeparator("data.tsv"), "\t")
  expect_equal(getSeparator("data.txt"), "\t")
  expect_equal(getSeparator("data.csv"), ",")
})

test_that("getSeparator errors for an unrecognised extension", {
  expect_error(getSeparator("data.xyz"), "Unknown separator")
})

# stringsToNamedVector()

test_that("stringsToNamedVector names elements from their own prettified values by default", {
  result <- stringsToNamedVector("foo_bar,baz_qux")

  expect_equal(unname(result), c("foo_bar", "baz_qux"))
  expect_equal(names(result), c("Foo bar", "Baz qux"))
})

test_that("stringsToNamedVector uses a separate names string when supplied", {
  result <- stringsToNamedVector("a,b", names_string = "First,Second", prettify_names = FALSE)

  expect_equal(unname(result), c("a", "b"))
  expect_equal(names(result), c("First", "Second"))
})

test_that("stringsToNamedVector errors when elements and names differ in length", {
  expect_error(
    stringsToNamedVector("a,b,c", names_string = "First,Second"),
    "different length"
  )
})

test_that("stringsToNamedVector simplifies file paths to names when requested", {
  result <- stringsToNamedVector(
    "/path/to/sample_one.tsv,/path/to/sample_two.tsv",
    simplify_files = TRUE, prettify_names = FALSE
  )

  expect_equal(names(result), c("sample one", "sample two"))
})

# splitStringToFixedwidthLines()

test_that("splitStringToFixedwidthLines wraps a long string at word boundaries", {
  result <- splitStringToFixedwidthLines("once upon a time there was a giant", linewidth = 10)

  lines <- strsplit(result, "\n")[[1]]
  expect_true(length(lines) > 1)
  expect_equal(paste(lines, collapse = " "), "once upon a time there was a giant")
})

test_that("splitStringToFixedwidthLines leaves a short string on one line", {
  expect_equal(splitStringToFixedwidthLines("short string", linewidth = 30), "short string")
})

# na.replace()

test_that("na.replace substitutes NAs in a character vector", {
  expect_equal(na.replace(c("a", NA, "b")), c("a", "NA", "b"))
})

test_that("na.replace accepts a custom replacement value", {
  expect_equal(na.replace(c("a", NA), replacement = "missing"), c("a", "missing"))
})

test_that("na.replace preserves factor class and levels", {
  f <- factor(c("a", NA, "b"))
  result <- na.replace(f)

  expect_true(is.factor(result))
  expect_equal(as.character(result), c("a", "NA", "b"))
})
