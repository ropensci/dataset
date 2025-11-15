test_that("defined(Date) constructs and preserves values,
          can be coerced back", {
  d <- as.Date("2024-01-01") + 0:2
  d_def <- defined(d, label = "Date variable", unit = "day")

  # class inheritance
  expect_true(is.defined(d_def))
  expect_true(inherits(d_def, "haven_labelled_defined"))
  expect_true(inherits(d_def, "Date"))

  # metadata preserved
  expect_equal(var_unit(d_def), "day")
  expect_equal(var_label(d_def), "Date variable")

  # values preserved
  expect_equal(as.numeric(unclass(d_def)), as.numeric(unclass(d)))
})


test_that("as.Date coercion can strip attributes if needed", {
  d <- as.Date("2024-01-01") + 0:2
  d_def <- defined(d, label = "Date variable", unit = "day")

  stripped_attributes <- names(attributes(as.Date(d_def, TRUE)))
  expect_equal(stripped_attributes, "class")
})
