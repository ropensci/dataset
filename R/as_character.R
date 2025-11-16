#' Coerce a defined vector to character
#'
#' `as_character()` is the recommended method to convert a
#' [`defined()`][defined] vector into a character vector. It is metadata-aware
#' and provides explicit control over whether semantic attributes are preserved.
#'
#' If `preserve_attributes = TRUE`, the returned character vector retains
#' metadata attributes (`unit`, `concept`, `namespace`, `label`). The
#' `"defined"` class is always removed.
#'
#' If `preserve_attributes = FALSE` (default), a plain character vector is
#' returned with *all* metadata stripped.
#'
#' @details
#' Use \code{strip_attributes = TRUE} when flattening or preparing data for
#' external pipelines, but keep the default when working with defined
#' vectors directly.
#'
#' Base R's `as.character()` always drops all attributes and returns plain
#' character values. It is equivalent to:
#' `as_character(x, strip_attributes = TRUE)`.
#'
#' @param x A vector created with [defined()].
#' @param strip_attributes Logical; should semantic metadata attributes
#'   (such as \code{label}, \code{unit}, \code{definition}, and
#'   \code{namespace}) be removed from the returned vector?
#'   Defaults to \code{FALSE}.
#'
#' @param ... Reserved for potential future use.
#'
#' @return A character vector (plain or with attributes).
#'
#' @examples
#' x <- defined(c("apple", "banana"), label = "Fruit", unit = "kg")
#'
#' # Recommended:
#' as_character(x)
#'
#' # Preserve metadata:
#' as_character(x, strip_attributes = FALSE)
#'
#' # Base R:
#' as.character(x)
#'
#' @export
as_character <- function(x, ...) {
  UseMethod("as_character", x)
}

#' @rdname as_character
#' @export
as_character.haven_labelled_defined <- function(
    x,
    strip_attributes = TRUE,
    ...) {
  base <- vctrs::vec_data(x)
  out <- as.character(base)

  if (! strip_attributes) {
    attr(out, "label") <- attr(x, "label")
    attr(out, "unit") <- attr(x, "unit")
    attr(out, "concept") <- attr(x, "concept")
    attr(out, "namespace") <- attr(x, "namespace")
  }

  out
}

#' @rdname as_character
#' @description
#' Base R `as.character()` always strips the class and metadata.
#'
#' @export
as.character.haven_labelled_defined <- function(x, ...) {
  as.character(vctrs::vec_data(x))
}
