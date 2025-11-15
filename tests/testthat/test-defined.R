test_that("defined() constructor creates expected vectors", {
  z <- defined(c(1, 1, 1, 0, 0, 0),
    label = "",
    labels = c("F" = 0, "M" = 1, "_N" = 99),
    concept = "https://example.org/sex"
  )
  x <- defined(c(0, 1, 0, 1, 1, 0),
    label = "sex",
    labels = c("F" = 0, "M" = 1, "_N" = 99),
    concept = "https://example.org/sex"
  )
  v <- defined(c(1, 0),
    label = "sex",
    labels = c("F" = 0, "M" = 1, "_N" = 99),
    concept = "https://example.org/sex"
  )
  y <- defined(c(1, 1, 1, 0, 0, 0),
    label = "sex",
    labels = c("F" = 0, "M" = 1, "_N" = 99),
    concept = "https://example.org/sex"
  )
  expect_equal(c(1:3, y), c(1, 2, 3, 1, 1, 1, 0, 0, 0))
  expect_equal(c("a", "b", y), c("a", "b", as.character(y)))
  expect_equal(
    c(x, y),
    defined(c(0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0),
      label = "sex",
      labels = c("F" = 0, "M" = 1, "_N" = 99),
      concept = "https://example.org/sex"
    )
  )
  expect_equal(
    c(x, y, v),
    defined(c(0, 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0),
      label = "sex",
      labels = c("F" = 0, "M" = 1, "_N" = 99),
      concept = "https://example.org/sex"
    )
  )
  a <- defined(c("a", "b"), label = "letters")
  d <- defined(c("d", "e"), label = "letters")
  expect_equal(
    c(a, d),
    defined(c("a", "b", "d", "e"), label = "letters")
  )
})

test_that("defined() attributes can be retrieved", {
  numeric_vec <- defined(1:5,
    label = "Length",
    unit = "meters",
    concept = "http://example.org/length",
    namespace = "http://example.org/ns"
  )

  factor_vec <- defined(
    x = factor(c("low", "medium", "high")),
    label = "Severity",
    concept = "http://example.org/severity",
    namespace = "severity"
  )

  expect_true(is.defined(numeric_vec))
  expect_equal(var_label(numeric_vec), "Length")
  expect_equal(var_unit(numeric_vec), "meters")
  expect_equal(var_concept(numeric_vec), "http://example.org/length")
  expect_equal(var_namespace(factor_vec), "severity")
  expect_equal(as.numeric(numeric_vec), 1:5)
})

#-------------------------- logical ------------------------------

test_that("defined.logical() works with metadata and labels", {
  x <- defined(
    x = c(TRUE, FALSE, TRUE),
    label = "Flag",
    unit = "boolean",
    concept = "https://example.org/flag",
    namespace = "test"
  )

  expect_true(is.defined(x))
  expect_s3_class(x, "haven_labelled_defined")
  expect_s3_class(x, "logical")
  expect_equal(
    attr(x, "concept"),
    "https://example.org/flag"
  )


  expect_equal(var_label(x), "Flag")
  expect_equal(var_unit(x), "boolean")
  expect_equal(var_concept(x), "https://example.org/flag")
  expect_equal(var_namespace(x), "test")
})

test_that("defined.logical() forbids value labels", {
  expect_error(
    defined(
      x      = c(TRUE, FALSE),
      labels = c("no" = FALSE, "yes" = TRUE)
    ),
    "value labels are not supported for logical vectors"
  )
})

#-------------------- subsetting ------------------------
test_that("Subsetting defined vectors works correctly", {
  vec <- defined(100:110, label = "Measurement", unit = "kg")
  sub <- vec[1:3]
  one <- vec[[2]]

  expect_true(is.defined(sub))
  expect_equal(var_label(sub), "Measurement")
  expect_equal(var_unit(one), "kg")
  expect_equal(as_numeric(one), 101)
})

test_that("summary and print methods work as expected", {
  vec <- defined(1:3, label = "Depth", unit = "m")

  expect_output(summary(vec), "Depth \\(m\\)")
  expect_output(print(vec), "Measured in m")
})

test_that("coercion to base types works", {
  vec <- defined(1:3, label = "Count", unit = "n")

  expect_equal(as.numeric(vec), c(1, 2, 3))
  expect_equal(as_character(defined(c("a", "b"), label = "Letter")), c("a", "b"))

  # Check that coercion to numeric fails if not numeric
  nonnum <- defined(c("a", "b"), label = "Text")
  expect_error(as_numeric(nonnum), "underlying data is not numeric")
})

test_that("comparison operations work correctly", {
  a <- defined(1:3)
  b <- defined(3:1)

  expect_equal(a < b, c(TRUE, FALSE, FALSE))
  expect_equal(a == c(1, 2, 3), c(TRUE, TRUE, TRUE))
})

test_that("combining works only with identical metadata", {
  a <- defined(1:3,
    label = "height",
    unit = "m", concept = "def", namespace = "http://ns"
  )
  b <- defined(4:6,
    label = "height",
    unit = "m", concept = "def", namespace = "http://ns"
  )

  expect_equal(
    c(a, b),
    defined(1:6,
      label = "height", unit = "m",
      concept = "def", namespace = "http://ns"
    )
  )

  c_diff <- defined(4:6,
    label = "length",
    unit = "m", concept = "def", namespace = "http://ns"
  )
  expect_error(c(a, c_diff), "var_label must be identical")
})

test_that("type_sum returns defined", {
  x <- defined(1:3)
  expect_equal(type_sum(x), "defined")
})


test_that("c() combines compatible defined vectors", {
  a <- defined(1:3,
    label = "Length",
    unit = "meter",
    concept = "http://example.org/def",
    namespace = "http://example.org/"
  )

  b <- defined(4:6,
    label = "Length",
    unit = "meter",
    concept = "http://example.org/def",
    namespace = "http://example.org/"
  )

  ab <- c(a, b)

  expect_s3_class(ab, "haven_labelled_defined")
  expect_equal(length(ab), 6)
  expect_equal(var_label(ab), "Length")
  expect_equal(var_unit(ab), "meter")
  expect_equal(var_concept(ab), "http://example.org/def")
  expect_equal(var_namespace(ab), "http://example.org/")
})

test_that("c() throws error on mismatched labels", {
  a <- defined(1:3, label = "Height")
  b <- defined(4:6, label = "Length")

  expect_error(
    c(a, b),
    "var_label must be identical "
  )
})

test_that("c() throws error on mismatched units", {
  a <- defined(1:3, label = "Length", unit = "meter")
  b <- defined(4:6, label = "Length", unit = "centimeter")

  expect_error(
    c(a, b),
    "unit must be identical "
  )
})

test_that("c() throws error on mismatched concepts", {
  a <- defined(1:3, label = "Length", concept = "http://example.org/def1")
  b <- defined(4:6, label = "Length", concept = "http://example.org/def2")

  expect_error(
    c(a, b),
    "concept must be identical or NULL across inputs"
  )
})

test_that("c() throws error on mismatched namespaces", {
  a <- defined(1:3, label = "Length", namespace = "http://example.org/")
  b <- defined(4:6, label = "Length", namespace = "http://example.com/")

  expect_error(
    c(a, b),
    "namespace must be identical or NULL across inpu"
  )
})

test_that("c() throws error on mismatched value labels", {
  a <- defined(1:3,
    label = "Sex",
    labels = c("M" = 1, "F" = 2)
  )
  b <- defined(4:6,
    label = "Sex",
    labels = c("Male" = 1, "Female" = 2)
  )

  expect_error(
    c(a, b),
    "value labels must be identical"
  )
})
