test_that("dataset_title() returns the current title", {
  df <- dataset_df(data.frame(x = 1:3))
  dataset_title(df, overwrite = TRUE) <- "Test Dataset"
  expect_equal(dataset_title(df), "Test Dataset")
})

test_that("dataset_title<-() sets a title when none is present", {
  df <- dataset_df(data.frame(x = 1:3))
  dataset_title(df, overwrite = TRUE) <- "Initial Title"
  expect_equal(dataset_title(df), "Initial Title")
})

test_that("dataset_title<-() overwrites title when allowed", {
  df <- dataset_df(data.frame(x = 1:3))
  dataset_title(df, overwrite = TRUE) <- "Original Title"
  dataset_title(df, overwrite = TRUE) <- "New Title"
  expect_equal(dataset_title(df), "New Title")
})

test_that("dataset_title<-() does not overwrite by default", {
  df <- dataset_df(data.frame(x = 1:3))
  dataset_title(df, overwrite = TRUE) <- "Do Not Change"
  expect_warning(
    dataset_title(df) <- "New Attempt",
    regexp = "The dataset already has a title"
  )
  expect_equal(dataset_title(df), "Do Not Change")
})

test_that("dataset_title<-() sets ':tba' if value is NULL", {
  df <- dataset_df(data.frame(x = 1:3))
  dataset_title(df) <- NULL
  expect_equal(dataset_title(df), ":tba")
})

test_that("dataset_title<-() errors on character vector of length > 1", {
  df <- dataset_df(data.frame(x = 1:3))
  expect_error(
    dataset_title(df) <- c("Title A", "Title B"),
    regexp = "multiple titles"
  )
})

test_that("dataset_title<-() errors on non-dataset_df input", {
  x <- data.frame(x = 1:3)
  expect_error(dataset_title(x),
    regexp = "must be created with dataset"
  )
  expect_error(dataset_title(x) <- "Invalid",
    regexp = "must be created with dataset"
  )
})
