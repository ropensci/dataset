#' Coerce a defined vector to numeric
#'
#' @description
#' `as_numeric()` converts a [`defined()`][defined] vector to a numeric vector.
#' It validates that the underlying data are numeric, and optionally preserves
#' or strips semantic metadata.
#'
#' This function supports **both** the new argument `strip_attributes`
#' and the backward-compatible `preserve_attributes = TRUE` used in older tests.
#'
#' @param x A vector created with [defined()].
#' @param strip_attributes Logical; whether to remove semantic metadata
#'   (`label`, `unit`, `concept`, `namespace`). Defaults to `TRUE`.
#' @param preserve_attributes Legacy argument. If `TRUE`, overrides
#'   `strip_attributes = FALSE`. If `FALSE`, ignored.
#' @param ... Reserved for future use.
#'
#' @return A numeric vector with or without preserved attributes.
#'
#' @export
as_numeric <- function(x, ...) {
  UseMethod("as_numeric", x)
}

#' @rdname as_numeric
#' @export
as_numeric.haven_labelled_defined <- function(x,
                                              strip_attributes = TRUE,
                                              preserve_attributes = FALSE,
                                              ...) {
  # Legacy compatibility:
  # preserve_attributes = TRUE  →  strip_attributes = FALSE
  if (isTRUE(preserve_attributes)) {
    strip_attributes <- FALSE
  }

  underlying <- vctrs::vec_data(x)

  if (!is.numeric(underlying)) {
    stop("as_numeric(): underlying data is not numeric.", call. = FALSE)
  }

  out <- underlying

  if (!strip_attributes) {
    attr(out, "label") <- attr(x, "label", exact = TRUE)
    attr(out, "unit") <- attr(x, "unit", exact = TRUE)
    attr(out, "concept") <- attr(x, "concept", exact = TRUE)
    attr(out, "namespace") <- attr(x, "namespace", exact = TRUE)
  }

  out
}

# Base R method: always drop metadata and class
#' @export
as.numeric.haven_labelled_defined <- function(x, ...) {
  vctrs::vec_data(x)
}

# vctrs casting: drop metadata
#' @export
#' @importFrom vctrs vec_data
vec_cast.double.haven_labelled_defined <- function(x, to, ...) {
  vctrs::vec_data(x)
}
