test_that("prettifyVariablename works", {
  # Expect unpretty things to change
  expect_equal(prettifyVariablename('ugly_name_of_thing'), "Ugly name of thing")
  # Expect pretty things to remain unchanged
  expect_equal(prettifyVariablename('Ugly name of thing'), "Ugly name of thing")
})

test_that("ucFirst works", {
  # Expect lowercase things to change
  expect_equal(ucfirst('foo'), "Foo")
  # Expect already ucfirst to not change
  expect_equal(prettifyVariablename('FOO'), "FOO")
})