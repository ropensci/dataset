test_that("get_bibentry() throws error for non-dataset_df", {
  expect_error(get_bibentry(mtcars),
    regexp = "dataset has no dataset_bibentry"
  )
})

test_that("get_bibentry() returns the dataset_bibentry object", {
  orange_bibentry <- get_bibentry(orange_df)
  expect_equal(orange_bibentry$title, "Growth of Orange Trees")
  expect_equal(orange_bibentry$date, "1998")
})

test_that("set_bibentry() sets a new bibentry", {
  orange_df_2 <- orange_df
  new_bibentry <- dublincore(
    title = "Test",
    creator = person("Jane", "Doe"),
    dataset_date = 2013
  )
  set_bibentry(dataset = orange_df_2) <- new_bibentry
  expect_equal(get_bibentry(orange_df_2)$title, "Test")
})

test_that("set_bibentry(NULL) populates sensible defaults", {
  df <- dataset_df(data.frame(x = 1))
  # start with no bibentry
  attr(df, "dataset_bibentry") <- NULL
  # setting NULL triggers default dublincore(...)
  set_bibentry(df) <- NULL
  be <- get_bibentry(df)
  expect_equal(be$title, "Untitled Dataset")
  # year defaults to current year
  expect_equal(be$year, substr(as.character(Sys.time()), 1, 4))
})

test_that("set_bibentry() does not overwrite provided year", {
  df <- dataset_df(data.frame(x = 1))
  provided_year <- "2010"
  be <- dublincore(
    title = "Keep My Year",
    creator = person("Jane", "Doe"),
    dataset_date = provided_year
  )
  # ensure be$year is set by constructor to provided_year
  expect_equal(be$year, provided_year)
  set_bibentry(df) <- be
  expect_equal(get_bibentry(df)$year, provided_year)
})

test_that("set_bibentry() returns invisibly and updates only attributes", {
  df <- dataset_df(data.frame(x = 1))
  be <- dublincore(
    title = "Attr Only",
    creator = person("Jane", "Doe"),
    dataset_date = 2022
  )
  expect_invisible(set_bibentry(df) <- be)
  expect_equal(ncol(df), 2) # data part unchanged, but rowid added
  expect_equal(get_bibentry(df)$title, "Attr Only")
})

test_that("get/set bibentry roundtrip works with DataCite bibentry too", {
  df <- dataset_df(data.frame(x = 1))
  be <- datacite(
    Title = "DataCite Title",
    Creator = person("Jane", "Doe"),
    PublicationYear = "2021"
  )
  set_bibentry(df) <- be
  got <- get_bibentry(df)
  expect_equal(got$title, "DataCite Title")
  expect_equal(got$year, "2021")
})

test_that("as_dublincore/as_datacite list views reflect bibentry", {
  df <- dataset_df(data.frame(x = 1))
  be <- dublincore(
    title = "List View",
    creator = person("Jane", "Doe"),
    dataset_date = "2023",
    publisher = "DataPub"
  )
  set_bibentry(df) <- be

  dc_list <- as_dublincore(df, type = "list")
  expect_equal(dc_list$title, "List View")
  expect_equal(as.character(dc_list$creator), as.character(be$author))

  dcite_list <- as_datacite(df, type = "list")
  expect_equal(dcite_list$Title, "List View")
  expect_equal(as.character(dcite_list$Creator), as.character(be$author))
  expect_true(dcite_list$Publisher %in% c("DataPub", "")) # depending on defaults
})

test_that("get_bibentry() error message is informative", {
  # plain data.frame <U+2192> no attribute
  df <- data.frame(x = 1)
  expect_error(
    get_bibentry(df),
    regexp = "dataset has no dataset_bibentry attribute"
  )
})
