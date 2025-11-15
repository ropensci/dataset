# These methods are related to the defined() class but
# tested in a separate test suite.

test_that("as.character() drops class and metadata from
          defined vector", {
  x <- defined(c("apple", "banana", "cherry"),
    label = "Fruit",
    unit = "kg",
    concept = "http://example.org/fruit"
  )
  result <- as.character(x)
  expect_type(result, "character")
  expect_false(inherits(result, "haven_labelled_defined"))
  expect_false("unit" %in% names(attributes(result)))
})

test_that("as_character() without preserve_attributes drops
          metadata", {
  x <- defined(c("red", "green", "blue"),
    label = "Color",
    unit = "rgb",
    concept = "http://example.org/color"
  )
  result <- as_character(x)
  expect_type(result, "character")
  expect_false("unit" %in% names(attributes(result)))
  expect_false(inherits(result, "haven_labelled_defined"))
})

test_that("as_character() with preserve_attributes keeps metadata", {
  x <- defined(c("yes", "no"),
    label = "Binary",
    unit = "boolean",
    concept = "http://example.org/binary",
    namespace = "http://example.org/ns"
  )
  result <- as_character(x, preserve_attributes = TRUE)
  expect_type(result, "character")
  expect_equal(attr(result, "unit"), "boolean")
  expect_equal(
    attr(result, "concept"),
    "http://example.org/binary"
  )
  expect_equal(
    attr(result, "namespace"),
    "http://example.org/ns"
  )
})

# -------------------------------------------------------------------
# Additional tests for attribute stripping / preservation
# -------------------------------------------------------------------

test_that("as_character() strips ALL semantic attributes by default", {
  x <- defined(
    c("apple", "banana"),
    label = "Fruit",
    unit = "kg",
    concept = "http://example.org/fruit",
    namespace = "http://example.org/ns"
  )
  attr(x, "extra_attr") <- "xyz"

  out <- as_character(x) # default preserve_attributes = FALSE

  expect_type(out, "character")
  expect_false(inherits(out, "haven_labelled_defined"))

  # All semantic attributes removed
  expect_null(attr(out, "label"))
  expect_null(attr(out, "unit"))
  expect_null(attr(out, "concept"))
  expect_null(attr(out, "namespace"))

  # Non-semantic attributes also removed
  expect_null(attr(out, "extra_attr"))
})

test_that("as_character(preserve_attributes = TRUE) preserves only semantic attributes", {
  x <- defined(
    c("red", "green"),
    label = "Color",
    unit = "rgb",
    concept = "http://example.org/color",
    namespace = "http://example.org/ns"
  )
  attr(x, "extra_attr") <- "to_be_dropped"

  out <- as_character(x, preserve_attributes = TRUE)

  expect_type(out, "character")
  expect_false(inherits(out, "haven_labelled_defined"))

  # Semantic attributes preserved
  expect_equal(attr(out, "label"), "Color")
  expect_equal(attr(out, "unit"), "rgb")
  expect_equal(attr(out, "concept"), "http://example.org/color")
  expect_equal(attr(out, "namespace"), "http://example.org/ns")

  # Extra attribute should not be carried over
  expect_null(attr(out, "extra_attr"))
})

test_that("as.character() (base) always returns plain character with no attributes", {
  x <- defined(
    c("yes", "no"),
    label = "Binary",
    unit = "boolean",
    concept = "http://example.org/binary",
    namespace = "http://example.org/ns"
  )
  attr(x, "extra_attr") <- "something"

  out <- as.character(x)

  expect_type(out, "character")
  expect_false(inherits(out, "haven_labelled_defined"))

  # No attributes at all
  expect_equal(names(attributes(out)) %||% character(), character(0))
})
