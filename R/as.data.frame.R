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
            preserve_attributes = !strip_attributes
          )
        } else {
          # fallback: character with semantic awareness
          col <- as_character(
            col,
            preserve_attributes = !strip_attributes
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

#' Convert a dataset_df to a tibble
#'
#' @description
#' A thin wrapper around `as.data.frame.dataset_df()`,
#' returning a tibble.
#'
#' @inheritParams as.data.frame.dataset_df
#'
#' @return A tibble with optional metadata stripping.
#'
#' @importFrom tibble as_tibble
#' @method as_tibble dataset_df
#' @export
as_tibble.dataset_df <- function(x, ..., strip_attributes = TRUE) {
  df <- as.data.frame.dataset_df(x, strip_attributes = strip_attributes, ...)
  tibble::as_tibble(df)
}

#' @rdname as_tibble.dataset_df
#' @export
as.tibble.dataset_df <- as_tibble.dataset_df
