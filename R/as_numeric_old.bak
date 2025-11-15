#' Coerce a defined vector to numeric
#'
#' `as_numeric()` is the recommended method to convert a [`defined()`][defined]
#' vector to a numeric vector. It ensures the underlying data is numeric and can
#' optionally preserve semantic metadata.
#'
#' If `preserve_attributes = TRUE`, the returned vector retains the `unit`,
#' `concept`, and `namespace` attributes, but is no longer of class `"defined"`.
#' If `FALSE` (default), a base numeric vector is returned without metadata.
#'
#' For character-based `defined` vectors, an error is thrown to avoid invalid
#' coercion.
#'
#' @param x A vector created with [defined()].
#' @param preserve_attributes Logical. Whether to keep metadata attributes.
#'   Defaults to `FALSE`.
#' @param ... Reserved for future use.
#'
#' @return A numeric vector (either bare or with metadata, depending on the
#'   `preserve_attributes` argument).
#'
#' @seealso [as.character()], [strip_defined()]
#'
#' @examples
#' gdp <- defined(c(3897L, 7365L), label = "GDP", unit = "million dollars")
#'
#' # Drop all metadata
#' as_numeric(gdp)
#'
#' # Preserve unit and concept
#' as_numeric(gdp, preserve_attributes = TRUE)
#'
#' # Equivalence to base coercion (without metadata)
#' as.numeric(gdp)
#'
#' # Metadata-aware variant preferred in pipelines
#' attr(as_numeric(gdp, TRUE), "unit")
#' @export
as_numeric <- function(x, ...) {
  UseMethod("as_numeric", x)
}

#' @export
vec_cast.double.haven_labelled_defined <- function(x, to, ...) {
  vctrs::vec_data(x)
}


#' @export
#' @rdname as_numeric
#' @importFrom vctrs vec_data
as_numeric.haven_labelled_defined <- function(x,
                                              preserve_attributes = FALSE,
                                              ...) {
  if (!is.numeric(vctrs::vec_data(x))) {
    stop("as_numeric(): underlying data is not numeric.")
  }

  if (preserve_attributes) {
    strip_defined(x)
  } else {
    vctrs::vec_data(x)
  }
}

#' @rdname as_numeric
#' @exportS3Method as.numeric haven_labelled_defined
#' @description
#' Base R's `as.numeric()` does not support custom classes like `defined()`.
#' This method drops all metadata and class information, returning a plain
#' numeric vector. It is equivalent to `as_numeric(x, preserve_attributes = FALSE)`.
as.numeric.haven_labelled_defined <- function(x, ...) {
  unclass(vctrs::vec_data(x))
}

#' @title Cast defined vector to base numeric (double)
#'
#' @description
#' S3 method for [`vctrs::vec_cast()`] that converts a
#' `haven_labelled_defined` vector (created by [defined()]) to a base
#' `numeric` (double) vector, dropping all semantic metadata.
#'
#' @param x `haven_labelled_defined` vector (created by [defined()]).
#' @param to Target type (must be `double()`).
#' @param ... Ignored; reserved for future use.
#'
#' @return A plain numeric (double) vector.
#'
#' @examples
#' x <- defined(c(10, 20), unit = "kg")
#' vctrs::vec_cast(x, double())
#' as.numeric(x)
#' @export
#' @method vec_cast.double haven_labelled_defined
#' @importFrom vctrs vec_cast vec_data
vec_cast.double.haven_labelled_defined <- function(x, to, ...) {
  vctrs::vec_data(x)
}
