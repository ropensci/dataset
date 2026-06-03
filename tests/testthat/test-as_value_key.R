test_that("as_value_key() works with named vectors", {
  x <- c(
    R = "functional_programming",
    png = "visualisation"
  )

  out <- as_value_key(x)

  expect_type(out, "character")

  expect_named(
    out,
    c("R", "png")
  )

  expect_equal(
    unname(out),
    c(
      "functional_programming",
      "visualisation"
    )
  )
})

test_that("as_value_key() works with named lists", {
  x <- list(
    R = "functional_programming",
    png = "visualisation"
  )

  out <- as_value_key(x)

  expect_type(out, "character")

  expect_named(
    out,
    c("R", "png")
  )

  expect_equal(
    out[["R"]],
    "functional_programming"
  )
})

test_that("as_value_key() works with two-column tibbles", {
  x <- tibble::tibble(
    extension = c(
      "R",
      "png"
    ),
    activity = c(
      "functional_programming",
      "visualisation"
    )
  )

  out <- as_value_key(x)

  expect_type(out, "character")

  expect_named(
    out,
    c("R", "png")
  )

  expect_equal(
    out[["png"]],
    "visualisation"
  )
})

test_that("as_value_key() preserves character coercion", {
  x <- tibble::tibble(
    key = c("a", "b"),
    value = c(1, 2)
  )

  out <- as_value_key(x)

  expect_type(out, "character")

  expect_equal(
    out,
    c(
      a = "1",
      b = "2"
    )
  )
})

test_that("as_value_key() fails on unsupported inputs", {
  expect_error(
    as_value_key(1:10),
    "`x` must be"
  )

  expect_error(
    as_value_key(
      data.frame(
        a = 1,
        b = 2,
        c = 3
      )
    ),
    "`x` must be"
  )
})


test_that("as_value_key() fails on unsupported inputs", {
  expect_error(
    as_value_key(1:10),
    "`x` must be"
  )

  expect_error(
    as_value_key(
      data.frame(
        a = 1,
        b = 2,
        c = 3
      )
    ),
    "`x` must be"
  )
})

test_that("invert_value_key() inversion and roundtrip", {
  # one-to-many contextual membership mapping
  record_sets <- list(
    conceptualisation = c(
      "D:/_package/alpha",
      "D:/_markdown/alpha-methodology"
    ),
    betaR = c(
      "D:/_packages/beta",
      "D:/_packages/prebeta"
    )
  )

  # ------------------------------------------------------------
  # inversion
  # ------------------------------------------------------------

  inverted <- invert_value_key(record_sets)

  expect_s3_class(
    inverted,
    "tbl_df"
  )

  expect_true(
    all(
      c("key", "value") %in% names(inverted)
    )
  )

  expect_equal(
    nrow(inverted),
    4
  )

  expect_equal(
    inverted$key,
    c(
      "conceptualisation",
      "conceptualisation",
      "betaR",
      "betaR"
    )
  )

  expect_equal(
    inverted$value,
    c(
      "D:/_package/alpha",
      "D:/_markdown/alpha-methodology",
      "D:/_packages/beta",
      "D:/_packages/prebeta"
    )
  )

  # ------------------------------------------------------------
  # canonical roundtrip
  # ------------------------------------------------------------

  roundtrip <- as_value_key(inverted)

  expect_equal(
    roundtrip,
    as_value_key(record_sets)
  )
})
