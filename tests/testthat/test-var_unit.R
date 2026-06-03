test_that("var_unit() gets the unit attribute from a vector", {
  x <- 1:3
  attr(x, "unit") <- "meters"
  expect_equal(var_unit(x), "meters")
})

test_that("var_unit() returns NULL if no unit is set", {
  x <- 1:3
  expect_null(var_unit(x))
})

test_that("var_unit<- sets the unit on a vector", {
  x <- 1:3
  var_unit(x) <- "kg"
  expect_equal(attr(x, "unit"), "kg")
})

test_that("var_unit<- removes the unit if set to NULL", {
  x <- 1:3
  attr(x, "unit") <- "dollars"
  var_unit(x) <- NULL
  expect_null(attr(x, "unit"))
})

test_that("get_variable_units is an alias for var_unit", {
  x <- 1:3
  attr(x, "unit") <- "liters"
  expect_equal(get_variable_units(x), "liters")
})

test_that("unit_attribute gets the exact 'unit' attribute", {
  x <- 1:3
  attr(x, "unit") <- "seconds"
  expect_equal(unit_attribute(x), "seconds")
})

test_that("get_unit_attribute is an alias for unit_attribute", {
  x <- 1:3
  attr(x, "unit") <- "minutes"
  expect_equal(get_unit_attribute(x), "minutes")
})

test_that("unit_attribute<- sets a unit using the low-level setter", {
  x <- 1:3
  unit_attribute(x) <- "<U+00B0>C"
  expect_equal(attr(x, "unit"), "<U+00B0>C")
})

test_that("set_unit_attribute sets and removes units correctly", {
  x <- 1:3

  # Set
  y <- set_unit_attribute(x, "EUR")
  expect_equal(attr(y, "unit"), "EUR")

  # Remove
  z <- set_unit_attribute(x, NULL)
  expect_null(attr(z, "unit"))
})

test_that("set_unit_attribute throws for invalid input", {
  expect_error(
    set_unit_attribute(1:3, c("USD", "EUR")),
    "`unit` should be a single character string or NULL"
  )

  expect_error(
    set_unit_attribute(1:3, 42),
    "`unit` should be a single character string or NULL"
  )

  expect_error(
    set_unit_attribute(1:3, TRUE),
    "`unit` should be a single character string or NULL"
  )
})
