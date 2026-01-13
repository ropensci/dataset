test_that("print.bibrecord dispatches and adds contributor section", {
  rec <- bibrecord(
    title = "X",
    author = person("A", "B"),
    contributor = person("C", "D")
  )

  out <- capture.output(print(rec))
  expect_true(any(grepl("Contributors:", out)))
})


test_that("bibrecord() returns a bibentry-compatible object", {
  a <- person("Jane", "Doe", role = "cre")
  b <- person("Alice", "Smith", role = "dtm")

  rec <- bibrecord(
    title = "Sample Dataset",
    author = list(a),
    contributor = list(b),
    publisher = "Open Science Org",
    identifier = "doi:10.1234/test",
    date = "2023-12-31",
    subject = "Example"
  )

  expect_s3_class(rec, "bibrecord")
  expect_s3_class(rec, "bibentry")
  expect_equal(rec$title, "Sample Dataset")
  expect_equal(rec$publisher, "Open Science Org")
  expect_equal(rec$identifier, "doi:10.1234/test")
  expect_true(inherits(rec$author, "person"))
  expect_true(inherits(attr(rec, "contributor")[[1]], "person"))
})

test_that("bibrecord() infers year from date if missing", {
  rec <- bibrecord(
    title = "Auto Year Test",
    author = person("Jane", "Doe"),
    date = "2022-01-01"
  )
  expect_equal(rec$year, "2022")
})

test_that("print.bibrecord() prints without error and includes contributor", {
  a <- person("Jane", "Doe", role = "cre")
  b <- person("Alice", "Smith", role = "dtm")

  rec <- bibrecord(
    title = "Printable Record",
    author = list(a),
    contributor = list(b),
    date = "2021-01-01"
  )

  expect_output(print(rec), "Contributors:")
  expect_output(print(rec), "Alice Smith")
})

test_that("bibrecord() works without contributor field", {
  a <- person("Jane", "Doe", role = "cre")

  rec <- bibrecord(
    title = "No Contributor",
    author = list(a),
    date = "2020-12-01"
  )

  expect_null(attr(rec, "contributor"))
  expect_output(print(rec), "Doe J")
})
