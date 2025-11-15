# These methods are related to the defined() class but
# tested in a separate test suite.

test_that("as_numeric() drops metadata by default (strip_attributes = TRUE)", {
  gdp <- defined(
    c(3897L, 7365L),
    label = "GDP",
    unit = "million dollars",
    concept = "http://example.org/GDP",
    namespace = "http://example.org/ns/"
  )

  num <- as_numeric(gdp)

  expect_type(num, "integer")
  expect_null(attr(num, "label"))
  expect_null(attr(num, "unit"))
  expect_null(attr(num, "concept"))
  expect_null(attr(num, "namespace"))
  expect_false(inherits(num, "haven_labelled_defined"))
})


test_that("as_numeric() preserves metadata when preserve_attributes = TRUE", {
  gdp <- defined(
    c(1000L, 2000L),
    label = "GDP",
    unit = "million dollars",
    concept = "http://example.org/GDP",
    namespace = "http://ns/"
  )

  num <- as_numeric(gdp, preserve_attributes = TRUE)

  expect_equal(attr(num, "label"), "GDP")
  expect_equal(attr(num, "unit"), "million dollars")
  expect_equal(attr(num, "concept"), "http://example.org/GDP")
  expect_equal(attr(num, "namespace"), "http://ns/")
  expect_type(num, "integer")

  # Should no longer be a defined vector
  expect_false(inherits(num, "haven_labelled_defined"))
})


test_that("as_numeric() preserves metadata when strip_attributes = FALSE", {
  gdp <- defined(
    c(10, 20, 30),
    label = "GDP",
    unit = "EUR",
    concept = "c:gdp"
  )

  num <- as_numeric(gdp, strip_attributes = FALSE)

  expect_equal(attr(num, "label"), "GDP")
  expect_equal(attr(num, "unit"), "EUR")
  expect_equal(attr(num, "concept"), "c:gdp")
})


test_that("preserve_attributes overrides strip_attributes", {
  x <- defined(
    c(1, 2, 3),
    label = "Test",
    unit = "kg"
  )

  out <- as_numeric(x, strip_attributes = TRUE, preserve_attributes = TRUE)

  expect_equal(attr(out, "label"), "Test")
  expect_equal(attr(out, "unit"), "kg")
})


test_that("as.numeric() base method always drops metadata and class", {
  gdp <- defined(c(1, 2, 3),
    label = "GDP",
    unit = "million USD",
    concept = "x"
  )

  rawnum <- as.numeric(gdp)

  expect_equal(rawnum, c(1, 2, 3))
  expect_type(rawnum, "double")

  expect_null(attributes(rawnum))
})


test_that("as_numeric() errors on non-numeric underlying data", {
  x <- defined(
    c("a", "b", "c"),
    label = "Letters"
  )
  expect_error(as_numeric(x), "underlying data is not numeric")
})


test_that("as_numeric() works with double underlying types", {
  x <- defined(
    c(1.1, 2.2, 3.3),
    label = "Float"
  )

  y <- as_numeric(x)

  expect_type(y, "double")
  expect_equal(y, c(1.1, 2.2, 3.3))
})


test_that("vec_cast(double) drops metadata", {
  x <- defined(
    c(10, 20),
    label = "Test",
    unit = "kg"
  )

  y <- vctrs::vec_cast(x, double())

  expect_equal(y, c(10, 20))
  expect_null(attr(y, "label"))
  expect_null(attr(y, "unit"))
  expect_type(y, "double")
})


test_that("strip_attributes = FALSE keeps *all* metadata consistently", {
  x <- defined(
    c(5, 6, 7),
    label = "Var",
    unit = "U",
    concept = "C",
    namespace = "NS"
  )

  y <- as_numeric(x, strip_attributes = FALSE)

  expect_equal(attr(y, "label"), "Var")
  expect_equal(attr(y, "unit"), "U")
  expect_equal(attr(y, "concept"), "C")
  expect_equal(attr(y, "namespace"), "NS")
})


test_that("as_numeric() returns base numeric with no defined class", {
  x <- defined(1:3, label = "X")

  y <- as_numeric(x)

  expect_false(inherits(y, "haven_labelled_defined"))
  expect_false(inherits(y, "defined"))
  expect_type(y, "integer")
})

test_that("as_numeric() drops metadata by default", {
  gdp <- defined(c(3897L, 7365L),
    label = "GDP",
    unit = "million dollars"
  )
  num <- as_numeric(gdp)
  expect_type(num, "integer")
  expect_null(attr(num, "label"))
  expect_null(attr(num, "unit"))
  expect_false(inherits(num, "defined"))
})

test_that("as_numeric(preserve_attributes = TRUE) keeps only semantic attributes", {
  x <- defined(
    1:3,
    label = "Count",
    unit = "n",
    concept = "http://example.org/count",
    namespace = "http://example.org/ns"
  )
  # add an extra, non-semantic attribute to x
  attr(x, "extra_attr") <- "should_not_survive"

  out <- as_numeric(x, preserve_attributes = TRUE)

  expect_type(out, "integer")
  expect_false(inherits(out, "haven_labelled_defined"))

  # Semantic attributes preserved
  expect_equal(attr(out, "label"), "Count")
  expect_equal(attr(out, "unit"), "n")
  expect_equal(attr(out, "concept"), "http://example.org/count")
  expect_equal(attr(out, "namespace"), "http://example.org/ns")

  # Extra, non-semantic attribute should NOT be kept
  expect_null(attr(out, "extra_attr"))
})

test_that("as_numeric() strips ALL semantic attributes by default", {
  x <- defined(
    1:3,
    label = "Count",
    unit = "n",
    concept = "http://example.org/count",
    namespace = "http://example.org/ns"
  )

  out <- as_numeric(x) # default preserve_attributes = FALSE

  expect_type(out, "integer")
  expect_false(inherits(out, "haven_labelled_defined"))

  # All semantic attributes should be gone
  expect_null(attr(out, "label"))
  expect_null(attr(out, "unit"))
  expect_null(attr(out, "concept"))
  expect_null(attr(out, "namespace"))
})
