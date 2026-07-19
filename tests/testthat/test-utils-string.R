# simple_split()

test_that("simple_split splits a delimited string into a character vector", {
  expect_equal(simple_split("a,b,c"), c("a", "b", "c"))
})

test_that("simple_split supports a custom separator", {
  expect_equal(simple_split("a;b;c", sep = ";"), c("a", "b", "c"))
})

test_that("simple_split returns NA unchanged", {
  expect_equal(simple_split(NA), NA)
})

# file_extension()

test_that("file_extension extracts a lower-cased file extension", {
  expect_equal(file_extension("data.CSV"), "csv")
  expect_equal(file_extension("/some/path/data.tsv"), "tsv")
})

test_that("file_extension uses the last extension of a multi-dot filename", {
  expect_equal(file_extension("archive.tar.gz"), "gz")
})

# guess_separator()

test_that("guess_separator infers tab for tsv/txt and comma for csv", {
  expect_equal(guess_separator("data.tsv"), "\t")
  expect_equal(guess_separator("data.txt"), "\t")
  expect_equal(guess_separator("data.csv"), ",")
})

test_that("guess_separator errors for an unrecognised extension", {
  expect_error(guess_separator("data.xyz"), "Unknown separator")
})

# strings_to_named_vector()

test_that("strings_to_named_vector names elements from their own prettified values by default", {
  result <- strings_to_named_vector("foo_bar,baz_qux")

  expect_equal(unname(result), c("foo_bar", "baz_qux"))
  expect_equal(names(result), c("Foo bar", "Baz qux"))
})

test_that("strings_to_named_vector uses a separate names string when supplied", {
  result <- strings_to_named_vector("a,b", names_string = "First,Second", prettify_names = FALSE)

  expect_equal(unname(result), c("a", "b"))
  expect_equal(names(result), c("First", "Second"))
})

test_that("strings_to_named_vector errors when elements and names differ in length", {
  expect_error(
    strings_to_named_vector("a,b,c", names_string = "First,Second"),
    "different length"
  )
})

test_that("strings_to_named_vector simplifies file paths to names when requested", {
  result <- strings_to_named_vector(
    "/path/to/sample_one.tsv,/path/to/sample_two.tsv",
    simplify_files = TRUE, prettify_names = FALSE
  )

  expect_equal(names(result), c("sample one", "sample two"))
})

# split_string_to_fixed_width_lines()

test_that("split_string_to_fixed_width_lines wraps a long string at word boundaries", {
  result <- split_string_to_fixed_width_lines("once upon a time there was a giant", linewidth = 10)

  lines <- strsplit(result, "\n")[[1]]
  expect_true(length(lines) > 1)
  expect_equal(paste(lines, collapse = " "), "once upon a time there was a giant")
})

test_that("split_string_to_fixed_width_lines leaves a short string on one line", {
  expect_equal(split_string_to_fixed_width_lines("short string", linewidth = 30), "short string")
})

# na_replace()

test_that("na_replace substitutes NAs in a character vector", {
  expect_equal(na_replace(c("a", NA, "b")), c("a", "NA", "b"))
})

test_that("na_replace accepts a custom replacement value", {
  expect_equal(na_replace(c("a", NA), replacement = "missing"), c("a", "missing"))
})

test_that("na_replace preserves factor class and levels", {
  f <- factor(c("a", NA, "b"))
  result <- na_replace(f)

  expect_true(is.factor(result))
  expect_equal(as.character(result), c("a", "NA", "b"))
})
