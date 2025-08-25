test_that("returns :unas for NULL input", {
  expect_equal(fix_contributor(NULL), ":unas")
})

test_that("returns :unas for ':unas' input", {
  expect_equal(fix_contributor(":unas"), ":unas")
})

test_that("handles a single person with explicit role", {
  p <- person(given = "Jane", family = "Doe", role = "dtm")
  expect_equal(fix_contributor(p), "{Jane Doe [dtm]}")
})

test_that("handles a single person with no role (defaults to ctb)", {
  p <- person(given = "Jane", family = "Doe")
  expect_equal(fix_contributor(p), "{Jane Doe [ctb]}")
})

test_that("merges multiple roles for the same person", {
  p1 <- person(given = "Jane", family = "Doe", role = "dtm")
  p2 <- person(given = "Jane", family = "Doe", role = "ctb")
  expect_equal(fix_contributor(list(p1, p2)), "{Jane Doe [dtm, ctb]}")
})

test_that("handles a list of persons with mixed roles", {
  p1 <- person(given = "Jane", family = "Doe", role = "dtm")
  p2 <- person(given = "John", family = "Smith") # defaults to ctb
  expect_equal(
    fix_contributor(list(p1, p2)),
    "{Jane Doe [dtm]} and {John Smith [ctb]}"
  )
})

test_that("clean_person_name strips role suffixes from person", {
  p <- person(given = "Jane", family = "Doe", role = "dtm")
  expect_equal(clean_person_name(p), "Jane Doe")
})

test_that("clean_person_name handles simple strings", {
  expect_equal(clean_person_name("Jane Doe [ctb]"), "Jane Doe")
  expect_equal(clean_person_name("John Smith"), "John Smith")
})

test_that("map_role_to_schema maps known roles", {
  expect_equal(map_role_to_schema("cre"), "creator")
  expect_equal(map_role_to_schema("ctb"), "contributor")
  expect_equal(map_role_to_schema("dtm"), "editor") # or whatever mapping you want
})

test_that("map_role_to_schema defaults to 'contributor' for unknown roles", {
  expect_equal(map_role_to_schema("xyz"), "contributor")
})
