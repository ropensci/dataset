# These methods are related to the defined() class but
# tested in a separate test suite.

test_that("as_factor() converts defined vector with labels to factor", {
  x <- defined(
    c(0, 1, 1, 0),
    label = "Sex",
    labels = c(
      "Female" = 0,
      "Male" = 1
    )
  )
  f <- as_factor(x)

  expect_s3_class(f, "factor")
  expect_equal(levels(f), c("Female", "Male"))
  expect_equal(as.character(f), c(
    "Female", "Male",
    "Male", "Female"
  ))
})

test_that("as_factor() preserves order of labels", {
  x <- defined(
    c(2, 3, 1),
    label = "Priority",
    labels = c("Low" = 1, "Medium" = 2, "High" = 3)
  )
  f <- as_factor(x)
  expect_equal(levels(f), c("Low", "Medium", "High"))
  expect_equal(as.character(f), c("Medium", "High", "Low"))
})

test_that("as_factor() without labels returns numeric-like factor", {
  x <- defined(c(1, 2, 1, 3), label = "Unlabelled")
  f <- as_factor(x)
  expect_s3_class(f, "factor")
  expect_equal(as.character(f), c("1", "2", "1", "3"))
})

test_that("as_factor() preserves metadata when
          strip_attributes = FALSE", {
  x <- defined(
    c(0, 1, 0),
    label = "Yes/No",
    labels = c("No" = 0, "Yes" = 1),
    unit = "boolean",
    concept = "http://example.org/yesno",
    namespace = "http://example.org/ns/"
  )

  f <- as_factor(x, strip_attributes = FALSE)

  expect_s3_class(f, "factor")
  expect_equal(attr(f, "unit"), "boolean")
  expect_equal(attr(f, "concept"), "http://example.org/yesno")
  expect_equal(attr(f, "namespace"), "http://example.org/ns/")
  expect_false("haven_labelled_defined" %in% class(f))
})

test_that("as_factor() strips metadata when
          strip_attributes = TRUE", {
  x <- defined(
    c(1, 2, 1),
    label = "Test",
    labels = c("One" = 1, "Two" = 2),
    unit = "u",
    concept = "c",
    namespace = "ns"
  )

  f <- as_factor(x, strip_attributes = TRUE)

  expect_s3_class(f, "factor")
  expect_null(attr(f, "unit"))
  expect_null(attr(f, "concept"))
  expect_null(attr(f, "namespace"))
})


test_that("as_factor() works for defined character vectors", {
  x <- defined(
    c("low", "medium", "high", "medium"),
    label = "Severity"
  )

  f <- as_factor(x)

  expect_s3_class(f, "factor")
  expect_equal(as.character(f), c("low", "medium", "high", "medium"))
  expect_equal(levels(f), sort(unique(c("low", "medium", "high"))))
})

test_that("as_factor() handles logical defined vectors", {
  x <- defined(
    c(TRUE, FALSE, TRUE),
    label = "Flag"
  )

  f <- as_factor(x)

  expect_s3_class(f, "factor")
  expect_equal(as.character(f), c("TRUE", "FALSE", "TRUE"))
  expect_equal(levels(f), c("FALSE", "TRUE"))
})

test_that("as_factor() handles missing values correctly", {
  x <- defined(
    c(1, NA, 2, NA),
    labels = c("One" = 1, "Two" = 2)
  )

  f <- as_factor(x)

  expect_s3_class(f, "factor")
  expect_equal(as.character(f), c("One", NA, "Two", NA))
})

test_that("as_factor() works with empty vectors", {
  x <- defined(
    numeric(0),
    label = "Empty"
  )

  f <- as_factor(x)

  expect_s3_class(f, "factor")
  expect_equal(length(f), 0)
  expect_equal(levels(f), character(0))
})

test_that("as_factor() errors gracefully when input is not defined", {
  expect_error(
    as_factor(1:3),
    "no applicable method"
  )
})

test_that("as_factor() retains level order stability for
          numeric but unlabeled", {
  x <- defined(
    c(10, 20, 10, 30),
    label = "Numbers!"
  )

  f <- as_factor(x)

  expect_equal(levels(f), c("10", "20", "30"))
})

