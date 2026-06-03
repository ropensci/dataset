test_that("var_labels() retrieves all variable labels", {
  df <- dataset_df(
    id = defined(1:3, label = "Observation ID"),
    temp = defined(c(22.5, 23.0, 21.8), label = "Temperature (<U+00B0>C)"),
    site = defined(c("A", "B", "A"))
  )

  labs <- var_labels(df)
  expect_type(labs, "list")
  expect_equal(labs$id, "Observation ID")
  expect_equal(labs$temp, "Temperature (<U+00B0>C)")
  expect_null(labs$site) # unlabeled by default
})

test_that("var_labels(null_action) handles fill, skip, na, empty", {
  df <- dataset_df(
    id = defined(1:3, label = "Observation ID"),
    site = defined(c("A", "B", "A"))
  )

  expect_equal(
    var_labels(df, null_action = "fill")$site,
    "site"
  )
  expect_false("site" %in% names(var_labels(df, null_action = "skip")))
  expect_true(is.na(var_labels(df, null_action = "na")$site))
  expect_equal(var_labels(df, null_action = "empty")$site, "")
})


test_that("var_labels<- sets multiple labels from list", {
  df <- dataset_df(
    id = defined(1:3),
    site = defined(c("A", "B", "A"))
  )
  var_labels(df) <- list(id = "Identifier", site = "Site code")

  expect_equal(var_label(df$id), "Identifier")
  expect_equal(var_label(df$site), "Site code")
  # Check dataset-level var_labels attribute
  expect_equal(attr(df, "var_labels")$id, "Identifier")
})

test_that("var_labels<- sets from named character vector", {
  df <- dataset_df(
    id = defined(1:3),
    site = defined(c("A", "B", "A"))
  )
  var_labels(df) <- c(id = "Identifier", site = "Site code")

  expect_equal(var_label(df$id), "Identifier")
  expect_equal(var_label(df$site), "Site code")
})

test_that("var_labels<- ignores non-matching names", {
  df <- dataset_df(
    id = defined(1:3),
    site = defined(c("A", "B", "A"))
  )
  var_labels(df) <- list(nonexistent = "Should be ignored", id = "Identifier")
  expect_equal(var_label(df$id), "Identifier")
  expect_null(var_label(df$site))
})

test_that("var_labels<- errors if character vector is unnamed", {
  df <- dataset_df(
    id = defined(1:3),
    site = defined(c("A", "B", "A"))
  )
  expect_error(var_labels(df) <- c("a", "b"), "must be named")
})


test_that("var_labels(unlist = TRUE) returns named vector", {
  df <- dataset_df(
    rowid = defined(1:3, label = "Observation ID"),
    site = defined(c("A", "B", "A"))
  )
  labs <- var_labels(df, unlist = TRUE, null_action = "empty")
  expect_type(labs, "character")
  expect_named(labs, c("rowid", "site"))
})
