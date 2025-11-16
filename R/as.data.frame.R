#' Convert a \code{dataset_df} to a base \code{data.frame}
#'
#' @description
#' Converts a [`dataset_df`] into a plain `data.frame`. By default this
#' strips semantic metadata (label, unit, concept/definition, namespace) from
#' each column, but this can be controlled via the
#' \code{strip_attributes} argument.
#'
#' Dataset-level metadata remains attached as inert attributes.
#'
#' @param x A [`dataset_df`].
#' @param strip_attributes Logical: should column-level semantic metadata
#'   be stripped? Default: \code{TRUE}.
#' @param ... Passed to \code{base::as.data.frame()}.
#' @inheritParams base::as.data.frame
#'
#' @return A base R `data.frame` without the \code{dataset_df} class.
#'
#' @rdname as.data.frame.dataset_df
#' @export
as.data.frame.dataset_df <- function(x, ...,
                                     strip_attributes = TRUE,
                                     optional = FALSE,
                                     stringsAsFactors = FALSE) {
  # ---- base data.frame conversion ----
  df <- base::as.data.frame(
    unclass(x),
    optional = optional,
    stringsAsFactors = stringsAsFactors
  )

  # ---- process each column ----
  df <- lapply(df, function(col) {
    if (inherits(col, "haven_labelled_defined")) {
      ## ---- Date / POSIXct cases ----
      if (inherits(col, "POSIXct")) {
        # keep underlying time values, drop wrapper class
        base <- structure(
          unclass(col),
          class = c("POSIXct", "POSIXt"),
          tzone = attr(col, "tzone", exact = TRUE)
        )

        if (!strip_attributes) {
          # preserve semantics if requested
          attr(base, "label") <- attr(col, "label", exact = TRUE)
          attr(base, "unit") <- attr(col, "unit", exact = TRUE)
          # support both old "definition" and new "concept" naming
          def_or_concept <- attr(col, "concept", exact = TRUE)
          if (is.null(def_or_concept)) {
            def_or_concept <- attr(col, "definition", exact = TRUE)
          }
          attr(base, "concept") <- def_or_concept
          attr(base, "namespace") <- attr(col, "namespace", exact = TRUE)
        }
        col <- base
      } else if (inherits(col, "Date")) {
        base <- structure(
          unclass(col),
          class = "Date"
        )

        if (!strip_attributes) {
          attr(base, "label") <- attr(col, "label", exact = TRUE)
          attr(base, "unit") <- attr(col, "unit", exact = TRUE)
          def_or_concept <- attr(col, "concept", exact = TRUE)
          if (is.null(def_or_concept)) {
            def_or_concept <- attr(col, "definition", exact = TRUE)
          }
          attr(base, "concept") <- def_or_concept
          attr(base, "namespace") <- attr(col, "namespace", exact = TRUE)
        }
        col <- base
      } else {
        ## ---- non-date/time defined vectors ----
        lbls <- attr(col, "labels", exact = TRUE)
        underlying <- vctrs::vec_data(col)

        if (!is.null(lbls)) {
          # labelled / categorical: go to character
          fac <- as_factor(col)     # returns factor with correct levels
          col <- as.character(fac)  # convert factor to characters
        } else if (is.numeric(underlying)) {
          # numeric: use semantic-aware numeric coercion
          col <- as_numeric(
            col,
            strip_attributes = strip_attributes
          )
        } else {
          # fallback: character with semantic awareness
          col <- as_character(
            col,
            strip_attributes = strip_attributes
          )
        }
      }

      # Ensure the "haven_labelled_defined" wrapper class is gone
      if (inherits(col, "haven_labelled_defined")) {
        class(col) <- setdiff(class(col), "haven_labelled_defined")
      }
    }

    # ---- final attribute stripping if requested ----
    if (strip_attributes) {
      # Handle both legacy "definition" and current "concept"
      attributes(col)[c("label", "unit", "concept", "definition", "namespace")] <- NULL
    }

    col
  })

  df <- as.data.frame(df,
                      stringsAsFactors = stringsAsFactors,
                      optional = optional
  )

  # ---- preserve dataset-level metadata (inert) ----
  for (att in c("dataset_bibentry", "subject", "prov")) {
    if (!is.null(attr(x, att))) {
      attr(df, att) <- attr(x, att)
    }
  }

  df
}

#' Coerce a `dataset_df` to a tibble
#'
#' @description
#' [as_tibble()] method for `dataset_df` objects.
#'
#' Converts a `dataset_df` into a tibble. By default, additional metadata
#' attached to the object is removed unless `strip_attributes = FALSE` is
#' specified.
#'
#' @rdname as_tibble.dataset_df
#' @name as_tibble.dataset_df
#'
#' @return
#' A [`tibble`][tibble::tibble()] containing the data (and optionally some
#' attributes) of the `dataset_df`.
#'
#' @param x A `dataset_df` object.
#' @inheritParams as.data.frame.dataset_df
#' @param ... Additional arguments passed to [as.data.frame.dataset_df()].
#' @param .name_repair Treatment of problematic column names:
#'   * `"minimal"`: No name checks.
#'   * `"unique"`: Enforce uniqueness.
#'   * `"check_unique"`: (default) require unique names.
#'   * `"universal"`: Make unique and syntactic.
#'   * A function or purrr-style anonymous function, see [rlang::as_function()].
#'
#' @seealso
#' * [tibble::as_tibble()]
#' * [as.data.frame.dataset_df()]
#'
#' @family tibble.methods
#'
#' @section Metadata handling:
#' The `strip_attributes` argument controls which attributes of the `dataset_df`
#' object are preserved. When `strip_attributes = TRUE` (the default), only
#' column-level information is kept.
#'
#' @section About column names:
#' The `dataset_df` class may internally use reserved names for indexing,
#' identifiers, or metadata. These are never exposed in the resulting tibble.
#' Column names in the output tibble may be repaired according to
#' `.name_repair`.
#'
#' @examples
#' # Convert a dataset_df to a tibble
#' x <- dataset_df(orange_df)
#' as_tibble(x)
#'
#' # Keep attributes
#' as_tibble(x, strip_attributes = FALSE)
#'
#' @export
as_tibble <- function(x, ...) {
  UseMethod("as_tibble")
}

#' @rdname as_tibble.dataset_df
#' @export
as_tibble.dataset_df <- function(
    x,
    ...,
    strip_attributes = TRUE,
    .name_repair = "check_unique"
) {
  df <- as.data.frame.dataset_df(
    x,
    strip_attributes = strip_attributes,
    ...
  )

  tibble::as_tibble(df, .name_repair = .name_repair)
}

#' @rdname as_tibble.dataset_df
#' @export
as.tibble.dataset_df <- as_tibble.dataset_df
