test_that("datacite_to_triples errors if title missing", {
  dc <- datacite(
    Title = "",
    Creator = person("Jane", "Doe", role = "cre"),
    Publisher = "ACME"
  )
  expect_error(datacite_to_triples(dc), "title is required")
})

test_that("datacite_to_triples includes core fields", {
  dc <- datacite(
    Title = "Climate Data",
    Creator = person("Jane", "Doe", role = "cre"),
    Publisher = "ACME",
    Identifier = "10.1234/xyz",
    Language = "en",
    Rights = "CC-BY-4.0",
    Description = "Test dataset",
    Format = "text/csv",
    Version = "1.0.0"
  )
  triples <- datacite_to_triples(dc)

  expect_true(any(grepl("title", triples)))
  expect_true(any(grepl("creator", triples)))
  expect_true(any(grepl("publisher", triples)))
  expect_true(any(grepl("identifier", triples)))
  expect_true(any(grepl("language", triples)))
  expect_true(any(grepl("rights", triples)))
  expect_true(any(grepl("descriptions", triples)))
  expect_true(any(grepl("formats", triples)))
  expect_true(any(grepl("version", triples)))
})

test_that("datacite_to_triples flattens subject to terms", {
  dc <- datacite(
    Title = "Biodiversity Data",
    Creator = person("Jane", "Doe", role = "cre"),
    Publisher = "ACME",
    Subject = subject_create("Forests", subjectScheme = "LCSH")
  )
  triples <- datacite_to_triples(dc)
  expect_true(any(grepl("subjects", triples)))
  expect_true(any(grepl("Forests", triples)))
})

test_that("datacite_to_triples uses structured relation attribute", {
  dc <- datacite(
    Title = "Climate Data",
    Creator = person("Jane", "Doe", role = "cre"),
    Publisher = "ACME"
  )

  df <- dataset_df(data.frame(x = 1), dataset_bibentry = dc)

  # Set relation on the dataset_df (works via setter)
  relation(df) <- related_create(
    relatedIdentifier = "10.9999/abc",
    relationType = "IsPartOf",
    relatedIdentifierType = "DOI"
  )

  # Extract bibentry back out
  dc <- get_bibentry(df)
  triples <- datacite_to_triples(dc)

  expect_true(any(grepl("relatedIdentifier", triples)))
  expect_true(any(grepl("relationType", triples)))
  expect_true(any(grepl("relatedIdentifierType", triples)))
})


test_that("datacite_to_triples handles multiple related items", {
  df <- dataset_df(data.frame(x = 1),
    dataset_bibentry = datacite(
      Title = "Climate Data",
      Creator = person("Jane", "Doe", role = "cre"),
      Publisher = "ACME"
    )
  )

  relation(df) <- list(
    related_create("10.1111/one", "IsPartOf", "DOI"),
    related_create("https://example.com/rel", "References", "URL", "Text")
  )

  triples <- datacite_to_triples(get_bibentry(df))

  expect_true(sum(grepl("relatedIdentifier", triples)) >= 2)
  expect_true(any(grepl("IsPartOf", triples)))
  expect_true(any(grepl("References", triples)))
  expect_true(any(grepl("relatedIdentifierType", triples)))
  expect_true(any(grepl("resourceTypeGeneral", triples)))
})

test_that("datacite_to_triples falls back to flat relatedidentifier", {
  # here you don<U+2019>t use the setter, so calling directly on the bibentry is fine
  dc <- datacite(
    Title = "Climate Data",
    Creator = person("Jane", "Doe", role = "cre"),
    Publisher = "ACME",
    RelatedIdentifier = "10.5555/fallback"
  )
  triples <- datacite_to_triples(dc)
  expect_true(any(grepl("10.5555/fallback", triples)))
})

test_that("datacite_to_triples handles mixed subject lists", {
  dc <- datacite(
    Title = "Biodiversity Data",
    Creator = person("Jane", "Doe", role = "cre"),
    Publisher = "ACME",
    Subject = list(
      "Forests",
      subject_create("Agriculture", subjectScheme = "LCSH")
    )
  )

  triples <- datacite_to_triples(dc)

  expect_true(any(grepl("subjects", triples)))
  expect_true(any(grepl("Forests", triples)))
  expect_true(any(grepl("Agriculture", triples)))
})
