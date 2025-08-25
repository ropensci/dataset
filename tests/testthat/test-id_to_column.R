test_that("id_to_column works on a basic data.frame", {
  df <- data.frame(a = 1:3, b = letters[1:3])
  out <- id_to_column(df)
  expect_true("rowid" %in% names(out))
  expect_equal(out$rowid, paste0("eg:", 1:3))
})

test_that("id_to_column replaces existing rowid column", {
  df <- data.frame(rowid = c("x", "y", "z"), a = 1:3)
  out <- id_to_column(df, prefix = "custom:")
  expect_equal(out$rowid, paste0("custom:", 1:3))
})

test_that("id_to_column accepts custom ids", {
  df <- data.frame(a = 1:2)
  out <- id_to_column(df, prefix = "ex:", ids = c("foo", "bar"))
  expect_equal(out$rowid, c("ex:foo", "ex:bar"))
})

test_that("id_to_column errors on mismatched ids length", {
  df <- data.frame(a = 1:3)
  expect_error(
    id_to_column(df, ids = c("a", "b")),
    "ids must be of same lengths as nrow"
  )
})

test_that("id_to_column handles special characters in row names", {
  df <- data.frame(a = 1:2)
  rownames(df) <- c("first row", "second#row!")
  out <- id_to_column(df)
  expect_equal(out$rowid, c("eg:first-row", "eg:second-row-"))
})

test_that("id_to_column works with prefix = NULL", {
  df <- data.frame(a = 1:2)
  out <- id_to_column(df, prefix = NULL)
  expect_equal(out$rowid, c("1", "2"))
})

test_that("id_to_column preserves dataset_df structure", {
  df <- dataset_df(a = 1:2)
  out <- id_to_column(df)
  expect_s3_class(out, "dataset_df")
  expect_true("rowid" %in% names(out))
})

test_that("id_to_column preserves dataset_df metadata", {
  df <- dataset_df(
    a = 1:2,
    dataset_bibentry = dublincore(
      title = "Test",
      creator = person("A", "B")
    )
  )
  subject(df) <- subject_create(
    term = "Test",
    valueURI = "http://example.org/test"
  )
  attr(df, "prov") <- list(generated = "now")

  out <- id_to_column(df, ids = c("a", "b"))
  expect_equal(attr(out, "dataset_bibentry")$title, "Test")
  expect_equal(subject(out)$term, "Test")
  expect_equal(subject(out)$valueURI, "http://example.org/test")
  expect_equal(attr(out, "prov")$generated, "now")
})

test_that("id_to_column works on dataset_df with custom ids", {
  df <- dataset_df(a = 1:2)
  out <- id_to_column(df, prefix = "ex:", ids = c("x", "y"))
  expect_equal(out$rowid, c("ex:x", "ex:y"))
})

test_that("id_to_column handles prefix = NULL with existing rowid", {
  df <- data.frame(rowid = c("old1", "old2"), a = 1:2)
  out <- id_to_column(df, prefix = NULL)
  # Should replace rowid, no prefix applied
  expect_equal(out$rowid, c("1", "2"))
})

test_that("id_to_column works with dataset_df with no provenance", {
  df <- dataset_df(
    a = 1:2,
    dataset_bibentry = dublincore(title = "NoProv", creator = person("X", "Y"))
  )
  # Explicitly remove provenance to test NULL handling
  attr(df, "prov") <- NULL
  out <- id_to_column(df)
  expect_s3_class(out, "dataset_df")
  # Rowids should still be present
  expect_equal(out$rowid, c("eg:1", "eg:2"))
})

test_that("id_to_column final return path is exercised", {
  df <- data.frame(a = 1:2)
  # No rowid column means it should go through lhs/rhs binding and end with tmp
  out <- id_to_column(df, prefix = "z:")
  expect_equal(out$rowid, c("z:1", "z:2"))
  expect_equal(names(out)[1], "rowid")
})
