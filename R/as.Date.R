#' Coerce a defined Date vector to a base R Date
#'
#' @description
#' Coerces a [`haven_labelled_defined`] vector whose underlying type is
#' [`Date`] into a base R `Date` vector.
#'
#' This method preserves the underlying date values and, by default,
#' also retains any semantic metadata attached to the variable.
#'
#' @details
#' Use \code{strip_attributes = TRUE} when flattening or preparing data for
#' external pipelines, but keep the default when working with defined
#' vectors directly.
#'
#' Base R's [`as.Date()`] also works, as it dispatches to this method via
#' S3. However, using `as.Date()` on defined vectors is considered safe
#' because this method ensures metadata is handled predictably.
#'
#' @param x A vector created with [`defined()`] with underlying type
#'   \code{Date}.
#' @param strip_attributes Logical; should the semantic metadata attributes
#'   (label, unit, definition, namespace) be removed from the returned
#'   vector? Defaults to \code{FALSE}.
#'
#' @param ... Additional arguments passed to [base::as.Date()].
#'
#' @return A `Date` vector, optionally carrying semantic metadata.
#'
#' @examples
#' d <- defined(Sys.Date() + 0:2, label = "Observation date")
#'
#' # Recommended usage
#' as.Date(d)
#'
#' # Stripping metadata
#' as.Date(d, strip_attributes = TRUE)
#'
#' @seealso
#'   [`as.POSIXct()`],
#'   [`as_numeric()`], [`as_character()`], [`as_logical()`],
#'   [`defined()`]
#'
#' @export
as.Date.haven_labelled_defined <- function(x,
                                           strip_attributes = FALSE,
                                           ...) {
  if (!inherits(x, "Date")) {
    stop(
      "as.Date.haven_labelled_defined() requires underlying Date vector.",
      call. = FALSE
    )
  }

  if (strip_attributes) {
    attr(x, "label") <- NULL
    attr(x, "unit") <- NULL
    attr(x, "definition") <- NULL
    attr(x, "namespace") <- NULL
  }

  # Remove only the defined wrapper, keep the
  # real POSIXct classes + attributes
  class(x) <- setdiff(class(x), "haven_labelled_defined")
  x
}
