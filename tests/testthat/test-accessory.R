# prettifyVariablename()

test_that("prettifyVariablename works", {
  # Expect unpretty things to change
  expect_equal(prettifyVariablename("ugly_name_of_thing"), "Ugly name of thing")
  # Expect pretty things to remain unchanged
  expect_equal(prettifyVariablename("Ugly name of thing"), "Ugly name of thing")
})

# ucfirst()

test_that("ucFirst works", {
  # Expect lowercase things to change
  expect_equal(ucfirst("foo"), "Foo")
  # Expect already ucfirst to not change
  expect_equal(prettifyVariablename("FOO"), "FOO")
})

# nlines()

test_that("nlines works", {
  expect_equal(nlines("foo\nbar"), 2)
  expect_equal(nlines("foo\nbar\nfi"), 3)
})

# hiddenInput()

test_that("nlines works", {
  test_html <- "<input type='text' id='myid' value='foo' style='display: none;'>"
  expect_equal(as.character(hiddenInput("myid", "foo")), test_html)
})
