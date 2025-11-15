test_that("defined(POSIXct) preserves values, can be
          coerced back", {
  p <- as.POSIXct("2024-01-01 12:00:00",
    tz = "UTC"
  ) + (0:2) * 3600
  p_def <- defined(p, label = "Time variable", unit = "hour")

  expect_true(is.defined(p_def))
  expect_true(inherits(p_def, "haven_labelled_defined"))
  expect_true(inherits(p_def, "POSIXct"))

  expect_equal(var_unit(p_def), "hour")
  expect_equal(var_label(p_def), "Time variable")
  expect_equal(attr(as.POSIXct(p_def), "tzone"), "UTC")

  expect_equal(
    as.numeric(unclass(p_def)),
    as.numeric(unclass(p))
  )
})

test_that("as.Date coercion can strip attributes if needed", {
  p <- as.POSIXct("2024-01-01 12:00:00",
    tz = "UTC"
  ) + (0:2) * 3600
  p_def <- defined(p, label = "Time variable", unit = "hour")

  stripped_attributes <- names(attributes(as.POSIXct(p_def, TRUE)))
  expect_equal(stripped_attributes, c("class", "tzone"))
})
