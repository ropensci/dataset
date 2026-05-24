test_that("prelabel() creates prelabelled vectors", {
  survey_df <- data.frame(
    respondent_id = paste0(
      "respondent_",
      1:10
    ),
    sex = c(
      "M", "F", "female", "male", "M", "non_binary",
      "F", "female", "unknown", "male"
    ),
    stringsAsFactors = FALSE
  )

  sex_map <- c(
    M = "male", F = "female"
  )

  out <- prelabel(
    survey_df$sex,
    labels = sex_map
  )

  expect_s3_class(
    out,
    "prelabelled"
  )

  expect_equal(
    length(out),
    10
  )

  expect_true(
    !is.null(
      attr(out, "prelabel")
    )
  )
})

test_that(
  "prelabel() preserves unmatched values when unmatched = keep",
  {
    survey_df <- data.frame(
      sex = c(
        "m", "f", "non_binary", "unknown"
      ),
      stringsAsFactors = FALSE
    )

    sex_map <- c(
      m = "male",
      f = "female"
    )

    out <- prelabel(
      survey_df$sex,
      labels = sex_map,
      unmatched = "keep"
    )

    expect_s3_class(
      out,
      "prelabelled"
    )

    expect_equal(
      unname(attr(out, "prelabel")),
      c("male", "female", "non_binary", "unknown")
    )

    expect_equal(
      as.vector(out),
      survey_df$sex
    )
  }
)

test_that(
  "prelabel() converts unmatched values according to policy",
  {
    x <- c(
      "A",
      "B",
      "C",
      "UNKNOWN",
      NA_character_
    )

    code_map <- c(
      A = "alpha",
      B = "beta"
    )

    out <- prelabel(
      x,
      labels = code_map,
      unmatched = "na"
    )

    expect_equal(
      unname(attr(out, "prelabel")),
      c(
        "alpha",
        "beta",
        "C",
        "UNKNOWN",
        "<NA>"
      )
    )
  }
)

test_that(
  "prelabel() works with tibble semantic mappings",
  {
    x <- c(
      "A",
      "B",
      "A"
    )

    code_tbl <- tibble::tibble(
      observed = c(
        "A",
        "B"
      ),
      labelled = c(
        "alpha",
        "beta"
      )
    )

    out <- prelabel(
      x,
      labels = code_tbl
    )

    expect_s3_class(
      out,
      "prelabelled"
    )

    expect_equal(
      unname(attr(out, "prelabel")),
      c(
        "alpha",
        "beta"
      )
    )

    expect_equal(
      as.vector(out),
      x
    )
  }
)

test_that(
  "prelabel() works with named list semantic mappings",
  {
    x <- c(
      "A",
      "B",
      "A"
    )

    code_list <- list(
      A = "alpha",
      B = "beta"
    )

    out <- prelabel(
      x,
      labels = code_list
    )

    expect_s3_class(
      out,
      "prelabelled"
    )

    expect_equal(
      unname(attr(out, "prelabel")),
      c(
        "alpha",
        "beta"
      )
    )

    expect_equal(
      as.vector(out),
      x
    )
  }
)

test_that(
  "prelabel() preserves original observational values",
  {
    x <- c(
      "m",
      "f",
      "unknown",
      NA_character_
    )

    sex_map <- c(
      m = "male",
      f = "female"
    )

    out <- prelabel(
      x,
      labels = sex_map
    )

    expect_equal(
      as.vector(out),
      x
    )

    expect_s3_class(
      out,
      "prelabelled"
    )
  }
)


test_that("prelabel() stores labels as attribute", {
  x <- c(
    "M",
    "F"
  )

  sex_map <- c(
    m = "male",
    f = "female"
  )

  out <- prelabel(
    x,
    labels = sex_map
  )

  expect_named(
    attributes(out),
    c(
      "prelabel",
      "class"
    ),
    ignore.order = TRUE
  )
})


test_that("is.prelabelled()", {
  x <- c(
    "M",
    "F"
  )

  sex_map <- c(
    m = "male",
    f = "female"
  )

  out <- prelabel(
    x,
    labels = sex_map
  )

  expect_true(
    is.prelabelled(out)
  )
})


test_that(
  "as.character.prelabelled preserves semantic assertions and missingness",
  {
    x <- c(
      "A",
      "B",
      "C",
      "UNKNOWN",
      NA_character_
    )

    code_map <- c(
      A = "alpha",
      B = "beta"
    )

    out <- prelabel(
      x,
      labels = code_map,
      unmatched = "na"
    )

    ## Semantic vocabulary --------------------------------

    expect_equal(
      attr(out, "prelabel"),
      c(
        A = "alpha",
        B = "beta",
        C = "C",
        UNKNOWN = "UNKNOWN",
        "<NA>" = "<NA>"
      )
    )

    ## Operational semantic projection --------------------

    expect_equal(
      as.character(out),
      c(
        "alpha",
        "beta",
        "C",
        "UNKNOWN",
        NA_character_
      )
    )

    ## Original observations preserved --------------------

    expect_equal(
      as.vector(out),
      x
    )

    expect_s3_class(
      out,
      "prelabelled"
    )
  }
)


test_that(
  "semantic coercion methods preserve operational semantics and provenance",
  {
    x <- c(
      "A",
      "B",
      "C",
      "UNKNOWN",
      NA_character_
    )

    code_map <- c(
      A = "alpha",
      B = "beta"
    )

    out <- prelabel(
      x,
      labels = code_map,
      unmatched = "na"
    )

    expected_semantics <- c(
      "alpha",
      "beta",
      "C",
      "UNKNOWN",
      NA_character_
    )

    expected_prelabel <- c(
      A = "alpha",
      B = "beta",
      C = "C",
      UNKNOWN = "UNKNOWN",
      "<NA>" = "<NA>"
    )

    ## ----------------------------------------------------
    ## prelabel object integrity
    ## ----------------------------------------------------

    expect_s3_class(
      out,
      "prelabelled"
    )

    expect_equal(
      as.vector(out),
      x
    )

    expect_equal(
      attr(out, "prelabel"),
      expected_prelabel
    )

    ## ----------------------------------------------------
    ## base semantic coercion
    ## ----------------------------------------------------

    base_semantics <- as.character(out)

    expect_equal(
      base_semantics,
      expected_semantics
    )

    expect_false(
      inherits(
        base_semantics,
        "prelabelled"
      )
    )

    ## ----------------------------------------------------
    ## provenance-preserving semantic workspace
    ## ----------------------------------------------------

    semantic_workspace <- as_character(out)

    expect_equal(
      as.vector(semantic_workspace),
      expected_semantics
    )

    expect_equal(
      as.vector(attr(semantic_workspace, "original_values")),
      x
    )

    expect_equal(
      attr(
        semantic_workspace,
        "prelabel"
      ),
      expected_prelabel
    )

    expect_false(
      inherits(
        semantic_workspace,
        "prelabelled"
      )
    )
  }
)
