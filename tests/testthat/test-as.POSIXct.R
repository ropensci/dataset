test_that("defined(POSIXct) preserves values, can be
          coerced back", {
  p <- as.POSIXct("2024-01-01 12:00:00",
    tz = "UTC"
  ) + (0:2) * 3600
  p_def <- defined(p, label = "Time variable", unit = "hour")

  expect_true(is.defined(p_def))
  expect_true(inherits(p_def, "haven_labelled_defined"))
  expect_true(inherits(p_def, "POSIXct"))

  expect_equal(var_unit(p_def), "hour")
  expect_equal(var_label(p_def), "Time variable")
  expect_equal(attr(as.POSIXct(p_def), "tzone"), "UTC")

  expect_equal(
    as.numeric(unclass(p_def)),
    as.numeric(unclass(p))
  )
})

test_that("as.Date coercion can strip attributes if needed", {
  p <- as.POSIXct("2024-01-01 12:00:00",
    tz = "UTC"
  ) + (0:2) * 3600
  p_def <- defined(p, label = "Time variable", unit = "hour")

  stripped_attributes <- names(attributes(as.POSIXct(p_def, TRUE)))
  expect_equal(stripped_attributes, c("class", "tzone"))
})


test_that("as.POSIXct.haven_labelled_defined respects
          user-initiated tz change", {
  # base POSIXct in UTC
  base_time <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC")

  # wrap in defined()/haven_labelled_defined with metadata
  x <- defined(
    base_time,
    label = "Timestamp",
    unit  = "seconds"
  )

  target_tz <- "Europe/Budapest"

  # user explicitly requests a new time zone
  out <- as.POSIXct(x, tz = target_tz)

  # what base R would do without the wrapper
  expected <- base::as.POSIXct(base_time, tz = target_tz)

  # 1) Class and wrapper removal
  expect_s3_class(out, "POSIXct")
  expect_true(inherits(out, "POSIXt"))
  expect_false(inherits(out, "haven_labelled_defined"))

  # 2) Instant in time: same as base R's conversion
  expect_identical(as.numeric(out), as.numeric(expected))

  # 3) Time zone: whatever base R uses, we mirror
  expect_identical(attr(out, "tzone"), attr(expected, "tzone"))

  # 4) Metadata: by default strip_attributes = FALSE, so we keep it
  expect_identical(attr(out, "label"), "Timestamp")
  expect_identical(attr(out, "unit"),  "seconds")
})

test_that("as.POSIXct.haven_labelled_defined
          strips attributes when requested", {
  base_time <- as.POSIXct("2024-01-01 12:00:00",
                          tz = "UTC")

  x <- defined(
    base_time,
    label = "Timestamp",
    unit  = "seconds"
  )

  target_tz <- "Europe/Budapest"

  out <- as.POSIXct(x, tz = target_tz, strip_attributes = TRUE)
  expected <- base::as.POSIXct(base_time, tz = target_tz)

  # Still matches base R conversion
  expect_identical(as.numeric(out), as.numeric(expected))
  expect_identical(attr(out, "tzone"), attr(expected, "tzone"))

  # Metadata stripped
  expect_null(attr(out, "label"))
  expect_null(attr(out, "unit"))
})
