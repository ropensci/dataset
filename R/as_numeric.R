#' Coerce a defined vector to numeric
#'
#' @description
#' `as_numeric()` converts a [`defined()`][defined] vector to a numeric vector.
#' It validates that the underlying data are numeric, and optionally preserves
#' or strips semantic metadata.
#'
#' @details
#' Use \code{strip_attributes = TRUE} when flattening or preparing data for
#' external pipelines, but keep the default when working with defined
#' vectors directly.
#'
#' `as.numeric()` drops all metadata and returns only the numeric values in
#' a vector.
#'
#' @param x A vector created with [defined()].
#' @param strip_attributes Logical; whether to remove semantic metadata
#'   (`label`, `unit`, `concept`, `namespace`). Defaults to `TRUE`.
#' @param ... Reserved for future use.
#'
#' @return A numeric vector with or without preserved attributes.
#' @examples
#' x <- defined(
#'      1:3,
#'      label = "Count",
#'      unit = "n",
#'      concept = "http://example.org/count",
#'      namespace = "http://example.org/ns"
#' )
#'
#' as_numeric(x)
#'
#' @export
as_numeric <- function(x, ...) {
  UseMethod("as_numeric", x)
}

#' @rdname as_numeric
#' @export
as_numeric.haven_labelled_defined <- function(x,
                                              strip_attributes = TRUE,
                                              ...) {

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
