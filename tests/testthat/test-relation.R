test_that("relation() gets/sets structured and flat variants", {
  df <- dataset_df(data.frame(x = 1))

  # character <U+2192> normalized
  relation(df) <- "https://doi.org/10.1234/abc"
  r <- relation(df)
  expect_s3_class(r, "related")
  expect_equal(r$relatedIdentifier, "https://doi.org/10.1234/abc")
  expect_equal(get_bibentry(df)$relation, "https://doi.org/10.1234/abc")
  expect_equal(
    get_bibentry(df)$relatedidentifier,
    "https://doi.org/10.1234/abc"
  )

  # structured
  rel <- related_create("10.5678/def", "References", "DOI", "Text")
  relation(df) <- rel
  r2 <- relation(df)
  expect_equal(r2$relationType, "References")
  expect_equal(get_bibentry(df)$relation, "10.5678/def")
  expect_equal(get_bibentry(df)$relatedidentifier, "10.5678/def")
})


test_that("relation<- errors for invalid input types", {
  df <- dataset_df(data.frame(x = 1))
  expect_error(
    {
      relation(df) <- list(relatedIdentifier = "not-classed")
    },
    regexp = "value must be created with `related_create\\(\\)`"
  )
})

test_that("relation() falls back to bibentry when attribute missing", {
  df <- dataset_df(data.frame(x = 1))
  relation(df) <- related_create("10.1/xyz", "IsPartOf", "DOI")
  attr(df, "relation") <- NULL # remove structured attr
  got <- relation(df)
  expect_true(is.character(got))
  expect_equal(got, "10.1/xyz")
})

test_that("relation() handles multiple related objects", {
  df <- dataset_df(data.frame(x = 1))

  rels <- list(
    related_create("10.1111/one", "IsPartOf", "DOI"),
    related_create("https://example.com/rel", "References", "URL", "Text")
  )

  relation(df) <- rels

  r <- relation(df)

  # should be a list
  expect_type(r, "list")
  expect_length(r, 2)

  # each element should be a `related` object
  expect_true(all(vapply(r, is.related, logical(1))))

  # identifiers should match
  expect_equal(r[[1]]$relatedIdentifier, "10.1111/one")
  expect_equal(r[[2]]$relatedIdentifier, "https://example.com/rel")

  # bibentry slots should contain the flat identifiers
  be <- get_bibentry(df)
  expect_equal(
    be$relatedidentifier,
    c("10.1111/one", "https://example.com/rel")
  )
  expect_equal(be$relation, c("10.1111/one", "https://example.com/rel"))
})
