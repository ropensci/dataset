test_that("as.data.frame.dataset_df() returns
          a plain data.frame", {
  ds <- dataset_df(
    a = defined(1:3,
      label = "A label",
      unit = "cm",
      concept = "http://example.com/a"
    ),
    b = defined(c("x", "y", "z"),
      namespace = "http://example.com/ns/$1"
    )
  )

  df <- as.data.frame(ds)

  # Class should be plain data.frame
  expect_true(is.data.frame(df))
  expect_false(inherits(df, "dataset_df"))
  expect_false(any(vapply(
    df, inherits, logical(1),
    "haven_labelled_defined"
  )))

  # Structure and values preserved
  expect_equal(nrow(df), 3)
  expect_equal(ncol(df), 3)
  expect_equal(df$a, 1:3)
  expect_equal(df$b, c("x", "y", "z"))

  # Column attributes must be removed
  expect_null(attr(df$a, "label"))
  expect_null(attr(df$a, "unit"))
  expect_null(attr(df$a, "concept"))
  expect_null(attr(df$a, "namespace"))
})

test_that("as.data.frame.dataset_df() preserves
          high-level metadata (inert)", {
  ds <- dataset_df(a = 1:2)
  df <- as.data.frame(ds)

  # Metadata preserved as non-actionable attributes
  expect_true(!is.null(attr(df, "dataset_bibentry")))
  expect_true(!is.null(attr(df, "prov")))
  expect_true(!is.null(attr(df, "subject")))

  # But the object must not behave as dataset_df
  expect_false(is.dataset_df(df))
  expect_error(creator(df), "dataset")
})

test_that("as.data.frame.dataset_df()
          handles empty and edge cases", {
  ds_empty <- dataset_df()
  df_empty <- as.data.frame(ds_empty)

  expect_true(is.data.frame(df_empty))
  expect_equal(ncol(df_empty), 1) # rowid column only
  expect_equal(nrow(df_empty), 0)

  # With only rowid defined explicitly
  ds_rowid <- dataset_df(rowid = defined(c("eg:1", "eg:2")))
  df_rowid <- as.data.frame(ds_rowid)

  expect_equal(df_rowid$rowid, c("eg:1", "eg:2"))
})

test_that("as.data.frame.dataset_df()
          strips defined semantics correctly", {
  ds <- dataset_df(
    numeric_def = defined(1:3, unit = "kg"),
    char_def = defined(c("A", "B", "C"), label = "X"),
    factor_def = defined(as.factor(c("Low", "High", "Low")),
      namespace = "ns:"
    )
  )

  df <- as.data.frame(ds)

  expect_equal(df$numeric_def, 1:3)
  expect_equal(df$char_def, c("A", "B", "C"))
  expect_equal(df$factor_def, c("Low", "High", "Low"))

  # Important: no semantic attributes must remain
  for (col in df) {
    expect_null(attr(col, "unit"))
    expect_null(attr(col, "label"))
    expect_null(attr(col, "concept"))
    expect_null(attr(col, "namespace"))
  }
})

test_that("as_tibble.dataset_df() behaves
          like as.data.frame but returns tibble", {
  ds <- dataset_df(
    x = defined(1:3, label = "x"),
    y = defined(c("A", "B", "C"))
  )

  tbl <- as_tibble(ds)

  expect_true(tibble::is_tibble(tbl))
  expect_false(inherits(tbl, "dataset_df"))
  expect_equal(tbl$x, 1:3)
  expect_equal(tbl$y, c("A", "B", "C"))

  # No defined-class left
  expect_false(inherits(tbl$x, "haven_labelled_defined"))
})

test_that("as.data.frame.dataset_df() does not alter
          values of defined(Date/POSIXct)", {
  d <- Sys.Date() + 0:2
  p <- as.POSIXct("2024-01-01 12:00:00",
    tz = "UTC"
  ) + (0:2) * 3600

  ds <- dataset_df(
    d = defined(d),
    p = defined(p)
  )

  df <- as.data.frame(ds)

  expect_equal(df$d, d)
  expect_equal(df$p, p)
})

test_that("as.data.frame.dataset_df() drops only
          dataset_df-specific classes", {
  ds <- dataset_df(
    x = I(Orange[1:3, ]) # I() keeps inner data.frame
  )

  df <- as.data.frame(ds)

  expect_true(is.data.frame(df))
  expect_false(inherits(df, "dataset_df"))
  expect_true(is.data.frame(df$x))
})


test_that("as.data.frame.dataset_df(strip_attributes = TRUE)
          strips all semantic attributes", {
  d <- Sys.Date() + 0:2
  p <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC") + (0:2) * 3600

  ds <- dataset_df(
    num  = defined(1:3, label = "Num", unit = "kg", concept = "x"),
    chr  = defined(c("A", "B", "C"), label = "Letters", namespace = "http://ns"),
    date = defined(d, label = "DateLabel", unit = "day"),
    time = defined(p, label = "TimeLabel", unit = "hour")
  )

  df <- as.data.frame(ds, strip_attributes = TRUE)

  # Values preserved
  expect_equal(df$num, 1:3)
  expect_equal(df$chr, c("A", "B", "C"))
  expect_equal(df$date, d)
  expect_equal(df$time, p)

  # All defined-class removed
  expect_false(any(vapply(df, inherits, logical(1), "haven_labelled_defined")))

  # All metadata attributes removed
  for (col in df) {
    expect_null(attr(col, "label"))
    expect_null(attr(col, "unit"))
    expect_null(attr(col, "concept"))
    expect_null(attr(col, "namespace"))
  }
})

test_that("as.data.frame.dataset_df(strip_attributes = FALSE) keeps semantic attributes", {
  d <- Sys.Date() + 0:2
  p <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC") + (0:2) * 3600

  ds <- dataset_df(
    num  = defined(1:3, label = "Num", unit = "kg", concept = "x"),
    chr  = defined(c("A", "B", "C"), label = "Letters", namespace = "http://ns"),
    date = defined(d, label = "DateLabel", unit = "day"),
    time = defined(p, label = "TimeLabel", unit = "hour")
  )

  df <- as.data.frame(ds, strip_attributes = FALSE)

  ## ---- Value equality ignoring attributes ----
  # Use base coercers to drop attributes and compare pure values
  expect_equal(as.numeric(df$num), 1:3)
  expect_equal(as.character(df$chr), c("A", "B", "C"))
  expect_equal(attr(df$date, "unit"), "day")
  expect_equal(attr(df$time, "unit"), "hour")

  df_s <- as.data.frame(ds, strip_attributes = TRUE)
  expect_equal(as.Date(df_s$date), d)
  expect_equal(as.POSIXct(df_s$time), p)


  ## ---- Metadata preserved ----
  expect_equal(attr(df$num, "label"), "Num")
  expect_equal(attr(df$num, "unit"), "kg")
  expect_equal(attr(df$num, "concept"), "x")

  expect_equal(attr(df$chr, "label"), "Letters")
  expect_equal(attr(df$chr, "namespace"), "http://ns")

  expect_equal(attr(df$date, "label"), "DateLabel")
  expect_equal(attr(df$date, "unit"), "day")

  expect_equal(attr(df$time, "label"), "TimeLabel")
  expect_equal(attr(df$time, "unit"), "hour")

  ## ---- Wrapper class removed ----
  expect_false(any(vapply(
    df,
    inherits, logical(1),
    "haven_labelled_defined"
  )))
})

test_that("as.data.frame.dataset_df(strip_attributes = TRUE)
          does not keep semantic attributes", {
  d <- Sys.Date() + 0:2
  p <- as.POSIXct("2024-01-01 12:00:00", tz = "UTC") + (0:2) * 3600

  ds <- dataset_df(
    num  = defined(1:3, label = "Num", unit = "kg", concept = "x"),
    chr  = defined(c("A", "B", "C"), label = "Letters", namespace = "http://ns"),
    date = defined(d, label = "DateLabel", unit = "day"),
    time = defined(p, label = "TimeLabel", unit = "hour")
  )

  df_s <- as.data.frame(ds, strip_attributes = TRUE)
  expect_equal(as.Date(df_s$date), d)
  expect_equal(as.POSIXct(df_s$time), p)
  expect_null(attr(df_s, "unit"))
})

