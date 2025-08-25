test_that("crosswalk_bib works with minimal dublincore", {
  dc <- dublincore(
    title = "Minimal Example",
    creator = person("Jane", "Doe")
  )

  sch <- crosswalk_bib(dc, "schema.org")
  expect_equal(sch$name, "Minimal Example")
  expect_equal(sch$creator[[1]]$name, "Jane Doe")
  expect_equal(sch$`@type`, "Dataset")

  dsp <- crosswalk_bib(dc, "dataspice")
  expect_s3_class(dsp$biblio, "data.frame")
  expect_equal(dsp$biblio$title, "Minimal Example")
})

test_that("crosswalk_bib propagates relation attribute", {
  rel <- related_create(
    "https://doi.org/10.5678/def",
    "References", "DOI"
  )
  dc <- dublincore(
    title = "Crosswalk Relation",
    creator = person("Jane", "Doe"),
    relation = rel
  )

  sch <- crosswalk_bib(dc, "schema.org")
  expect_equal(sch$isBasedOn, "https://doi.org/10.5678/def")

  dsp <- crosswalk_bib(dc, "dataspice")
  expect_match(dsp$biblio$relation, "10.5678/def")
})

test_that("crosswalk_bib maps subject correctly", {
  s1 <- subject_create("Climate Change",
    schemeURI = "http://id.loc.gov/subjects"
  )
  dc <- dublincore(
    title = "Climate Crosswalk",
    creator = person("Eve", "Rivera"),
    subject = s1
  )

  sch <- crosswalk_bib(dc, "schema.org")
  expect_equal(sch$keywords, "Climate Change")
})

test_that("crosswalk_bib maps roles correctly", {
  dc <- dublincore(
    title = "Role Example",
    creator = list(
      person("Jane", "Doe", role = "cre"),
      person("John", "Smith", role = "ctb")
    ),
    publisher = "Open Data Inst.",
    description = "Role test"
  )

  sch <- crosswalk_bib(dc, "schema.org")
  expect_equal(sch$creator[[1]]$name, "Jane Doe")
  expect_equal(sch$creator[[1]]$role, "creator")
  expect_equal(sch$creator[[2]]$role, "contributor")
})
