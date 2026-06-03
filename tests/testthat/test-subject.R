test_that("subject() errors for non-dataset_df", {
  expect_error(
    subject(mtcars),
    "subject\\(x\\): x must be a dataset"
  )
})

test_that("subject() returns the structured subject
          from attribute when present", {
  df <- dataset_df(data.frame(x = 1:3))
  subject(df) <- subject_create("Biodiversity")
  s <- subject(df)
  expect_s3_class(s, "subject")
  expect_equal(s$term, "Biodiversity")
})

test_that("subject() returns the structured subject
          from attribute when present", {
  df <- dataset_df(data.frame(x = 1:3))
  subject(df) <- subject_create("Biodiversity")
  s <- subject(df)
  expect_s3_class(s, "subject")
  expect_equal(s$term, "Biodiversity")
})

test_that("subject() falls back to bibentry$subject when attr is missing", {
  df <- dataset_df(data.frame(x = 1:3))
  # set via character → bibentry$subject set to term + attr(subject) set
  subject(df) <- "Forestry"
  # remove attr to force fallback
  attr(df, "subject") <- NULL
  got <- subject(df)
  expect_equal(got, "Forestry")
})

test_that("subject() returns default subject for a fresh dataset_df", {
  df <- dataset_df(data.frame(x = 1:3))
  s <- subject(df)
  expect_s3_class(s, "subject")
  expect_equal(s$term, default_subject$term) # e.g., "Data sets"
})


test_that("subject() messages and returns NULL when subject truly missing", {
  df <- dataset_df(data.frame(x = 1:3))
  # Nuke both attr and bibentry entry to simulate a broken object
  attr(df, "subject") <- NULL
  be <- get_bibentry(df)
  be$subject <- NULL
  attr(df, "dataset_bibentry") <- be

  expect_message(out <- subject(df), "No subject is recorded\\.")
  expect_null(out)
})


# --- subject_create / new_Subject ---

test_that("subject_create() creates a valid subject object", {
  s <- subject_create(
    term = "Climate Data",
    subjectScheme = "LCSH",
    schemeURI = "http://id.loc.gov/authorities/subjects",
    valueURI = "https://id.loc.gov/authorities/subjects/sh2001000174",
    prefix = "lcsh:"
  )
  expect_s3_class(s, "subject")
  expect_equal(s$term, "Climate Data")
  expect_equal(s$subjectScheme, "LCSH")
  expect_equal(s$schemeURI, "http://id.loc.gov/authorities/subjects")
  expect_equal(s$valueURI, "https://id.loc.gov/authorities/subjects/sh2001000174")
  expect_equal(s$prefix, "lcsh:")
})

test_that("subject_create() handles NULL term gracefully", {
  s <- subject_create(term = NULL)
  expect_s3_class(s, "subject")
  expect_equal(s$term, ":tba")
})

test_that("subject_create() handles multiple terms", {
  terms <- c("Climate", "Agriculture")
  schemes <- c("LCSH", "LCSH")
  s <- subject_create(term = terms, subjectScheme = schemes)
  expect_type(s, "list")
  expect_s3_class(s[[1]], "subject")
  expect_equal(s[[1]]$term, "Climate")
  expect_equal(s[[2]]$term, "Agriculture")
})

test_that("new_Subject() constructs with/without classificationCode", {
  s1 <- new_Subject("Environment")
  expect_s3_class(s1, "subject")
  expect_equal(s1$term, "Environment")

  s2 <- new_Subject("Biology", classificationCode = "06")
  expect_s3_class(s2, "subject")
  expect_equal(s2$classificationCode, "06")
})

# --- is.subject ---

test_that("is.subject() returns TRUE for subject objects", {
  s <- subject_create("Health")
  expect_true(is.subject(s))
})

test_that("is.subject() returns FALSE for non-subject", {
  expect_false(is.subject(list(term = "NotClassed")))
  expect_false(is.subject("Just a string"))
})

# --- setter semantics ---

test_that("subject<- handles NULL and character input", {
  df <- dataset_df(data.frame(x = 1:3))

  subject(df) <- NULL
  expect_equal(subject(df)$term, ":tba")

  subject(df) <- "Oceanography"
  expect_equal(subject(df)$term, "Oceanography")
})

test_that("subject<- errors for invalid input types", {
  df <- dataset_df(data.frame(x = 1:3))
  expect_error(
    {
      subject(df) <- list(term = "Invalid")
    },
    regexp = "value must be created with"
  )
})


test_that("subject<- replaces both attr and bibentry field", {
  df <- dataset_df(data.frame(x = 1:3))
  s <- subject_create("Volcanology")
  subject(df) <- s

  be <- get_bibentry(df)

  expect_equal(subject(df)$term, "Volcanology")
  expect_s3_class(subject(df), "list")
})

# --- integration with as_datacite(dataset_df) flattening ---

test_that("as_datacite(dataset_df) flattens subject to its term", {
  df <- dataset_df(data.frame(x = 1))
  creator(df) <- person("Jane", "Doe", role = "cre")
  subject(df) <- subject_create(
    term = "Oranges",
    schemeURI = "http://id.loc.gov/authorities/subjects",
    valueURI = "http://id.loc.gov/authorities/subjects/sh85095257",
    subjectScheme = "LCCH",
    prefix = "lcch:"
  )
  out <- as_datacite(df, type = "dataset_df")
  expect_s3_class(out, "dataset_df")
  expect_equal(as.character(out$Subject), "Oranges")
})

test_that("subject<- handles multiple subjects as list", {
  df <- dataset_df(data.frame(x = 1:3))

  subject(df) <- subject_create(c("Forests", "Agriculture"))

  s <- attr(df, "subject")
  expect_type(s, "list")
  expect_s3_class(s[[1]], "subject")
  expect_equal(s[[1]]$term, "Forests")
  expect_equal(s[[2]]$term, "Agriculture")

  be <- get_bibentry(df)
  expect_true(all(c("Forests", "Agriculture") %in% be$subject))

  got <- subject(df)
  expect_type(got, "list")
  expect_equal(
    vapply(got, `[[`, "term", FUN.VALUE = character(1)),
    c("Forests", "Agriculture")
  )
})

