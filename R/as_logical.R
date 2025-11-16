#' Coerce a defined vector to logical
#'
#' @description
#' Coerces a [`haven_labelled_defined`] vector created with [`defined()`]
#' into a base R logical vector.
#'
#' This function is the recommended, semantic-aware interface for converting
#' defined logical vectors. It preserves the underlying logical values and,
#' unless otherwise requested, retains the semantic metadata attached to the
#' variable.
#'
#' @details
#' Use \code{strip_attributes = TRUE} when flattening or preparing data for
#' external pipelines, but keep the default when working with defined
#' vectors directly.
#'
#' Users may also call base R's [`as.logical()`], which will dispatch to this
#' method automatically via S3. However, `as_logical()` is preferred because
#' it makes the semantics explicit and supports the `strip_attributes`
#' argument.
#'
#' @param x A vector created with [`defined()`].
#' @param strip_attributes Logical; should semantic metadata attributes
#'   (such as \code{label}, \code{unit}, \code{definition}, and
#'   \code{namespace}) be removed from the returned vector?
#'   Defaults to \code{FALSE}.
#'
#'
#' @param ... Additional arguments (currently unused).
#'
#' @return A logical vector. If \code{strip_attributes = FALSE}, any semantic
#'   metadata attached to \code{x} will be carried over to the returned vector.
#'
#' @examples
#' # Basic usage
#' flg <- defined(c(TRUE, FALSE, TRUE), label = "Flag")
#' as_logical(flg)
#'
#' # Metadata preserved by default
#' attr(as_logical(flg), "label")
#'
#' # Stripping metadata
#' as_logical(flg, strip_attributes = TRUE)
#'
#' # Base R coercion also works (via S3 method dispatch)
#' as.logical(flg)
#'
#' @seealso
#'   [`as_numeric()`], [`as_character()`],
#'   [`as.Date()`], [`as.POSIXct()`],
#'   [`defined()`]
#'
#' @export
as_logical <- function(x, ...) {
  UseMethod("as_logical", x)
}

#' @rdname as_logical
#' @export
as_logical.haven_labelled_defined <- function(x,
                                              strip_attributes = TRUE,
                                              ...) {
  out <- as.logical(unclass(x))

  if (!strip_attributes) {
    attr(out, "label") <- attr(x, "label", exact = TRUE)
    attr(out, "unit") <- attr(x, "unit", exact = TRUE)
    attr(out, "definition") <- attr(x, "definition", exact = TRUE)
    attr(out, "namespace") <- attr(x, "namespace", exact = TRUE)
  }

  out
}
