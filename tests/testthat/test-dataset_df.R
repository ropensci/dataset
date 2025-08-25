test_that("dataset_df() creates and validates basic object", {
  df <- dataset_df(mtcars)
  expect_s3_class(df, "dataset_df")
  expect_true(is.dataset_df(df))
  expect_false(is.dataset_df(mtcars))

  expect_equal(get_bibentry(df)$author, person("Author", "Unknown"))
})

test_that("dataset_df() respects identifier for rowid creation", {
  df <- dataset_df(mtcars,
    identifier = c(mt = "http://mtcars.com/dataset#")
  )
  expect_s3_class(df$rowid, "haven_labelled_defined")
  expect_true(all(grepl("^mt", df$rowid)))
})


test_that("dataset_df() stores variable metadata", {
  df <- dataset_df(
    foo = defined(1:3, label = "Example Var", unit = "count")
  )
  expect_equal(var_label(df$foo), "Example Var")
  expect_equal(var_unit(df$foo), "count")
})

test_that("dataset_df() creates with full metadata", {
  bib <- dublincore(
    title = "Growth of Orange Trees",
    creator = person("N.R.", "Draper", role = "cre"),
    publisher = "Wiley",
    identifier = "https://doi.org/10.5281/zenodo.10396807",
    dataset_date = 1998,
    description = "35 rows of orange growth"
  )

  df <- dataset_df(
    tree = defined(Orange$Tree, label = "Tree ID"),
    age = defined(Orange$age, unit = "days"),
    circumference = defined(Orange$circumference, label = "Size"),
    dataset_bibentry = bib
  )

  expect_equal(dataset_title(df), "Growth of Orange Trees")
  expect_equal(creator(df)[[1]]$family, "Draper")
})

test_that("as_dataset_df() converts from data.frame", {
  df <- as_dataset_df(Orange)
  expect_s3_class(df, "dataset_df")
})

test_that("new_dataset() validates input", {
  expect_error(new_dataset(42, identifier = c(x = "http://example.com")))
  expect_s3_class(
    new_dataset(Orange, identifier = c(x = "http://example.com")),
    "dataset_df"
  )
})

test_that("subsetting preserves class and metadata", {
  df <- dataset_df(Orange)
  sub <- df[1:3, ]
  expect_s3_class(sub, "dataset_df")
  expect_true(!is.null(attr(sub, "dataset_bibentry")))
})

test_that("rowid not duplicated if already present", {
  df <- dataset_df(
    rowid = defined(paste0("id:", 1:3)),
    x = 1:3
  )
  expect_equal(names(df)[1], "rowid")
})

test_that("summary() outputs metadata", {
  df <- dataset_df(
    x = 1:3,
    dataset_bibentry = datacite(
      Title = "Summary Test",
      Creator = person("Jane", "Doe"),
      PublicationYear = 2023
    )
  )
  expect_output(summary(df), "Summary Test")
  expect_output(summary(df), "Doe")
})

test_that("print() outputs full APA-style citation and data", {
  df <- dataset_df(
    x = 1:2,
    dataset_bibentry = dublincore(
      title = "Tiny Data",
      creator = list(person("A.", "Smith")),
      identifier = "https://doi.org/10.1234/example",
      dataset_date = 2021
    )
  )
  expect_output(print(df), "Smith \\(2021\\): Tiny Data \\[dataset\\], https://doi.org/10.1234/example")
  expect_output(print(df), "1 obs1\\s+1")
  expect_output(print(df), "2 obs2\\s+2")
})

test_that("dataset_df supports multiple authors in APA formatting", {
  df <- dataset_df(
    x = 1:2,
    dataset_bibentry = dublincore(
      title = "Multi Author Example",
      creator = list(
        person("A.", "Alpha"),
        person("B.", "Beta"),
        person("C.", "Gamma")
      ),
      dataset_date = 2020
    )
  )
  expect_output(print(df), "Alpha et al. \\(2020\\)")
})

test_that("dataset_df supports institutional authors", {
  df <- dataset_df(
    x = 1:2,
    dataset_bibentry = dublincore(
      title = "Inst Author Example",
      creator = person(family = "OpenDataOrg"),
      dataset_date = 2022
    )
  )
  expect_output(print(df), "OpenDataOrg \\(2022\\)")
})

test_that("rbind works with bind_defined_rows()", {
  df1 <- dataset_df(a = defined(1:2, label = "A"))
  df2 <- dataset_df(a = defined(3:4, label = "A"))
  out <- bind_defined_rows(df1, df2)
  expect_equal(nrow(out), 4)
  expect_s3_class(out, "dataset_df")
})

test_that("names.dataset_df returns correct column names", {
  df <- dataset_df(a = 1:3, b = 4:6)
  expect_equal(names(df)[1], "rowid")
  expect_length(names(df), 3)
})

test_that("dataset_df handles empty input gracefully", {
  empty <- dataset_df()
  expect_s3_class(empty, "dataset_df")
  expect_equal(nrow(empty), 0)
})

test_that("dataset_df rejects invalid variable types", {
  expect_error(dataset_df(a = new.env()), "must be vectors")
})
