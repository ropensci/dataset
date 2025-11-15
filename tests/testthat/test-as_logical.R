test_that("as_logical coerces back booleans with or without
          semantic metadata", {
  x <- defined(
    x = c(TRUE, FALSE, TRUE),
    label = "Flag",
    unit = "boolean",
    concept = "https://example.org/flag",
    namespace = "test"
  )

  x_coerced <- as_logical(x, strip_attributes = FALSE)
  expect_equal(
    attr(x_coerced, "label"),
    "Flag"
  )

  expect_equal(as_logical(x), c(TRUE, FALSE, TRUE))
})
