#' Coerce a defined POSIXct vector to a base R POSIXct
#'
#' @description
#' Coerces a [`haven_labelled_defined`] vector whose underlying type is
#' [`POSIXct`] into a base R `POSIXct` time vector.
#'
#' This method preserves both the timestamp values and the original time zone.
#' By default, semantic metadata is also retained.
#'
#' Base R's [`as.POSIXct()`] also works, as it dispatches to this method via
#' S3. Using this method directly is preferred when metadata preservation
#' matters.
#'
#' @param x A vector created with [`defined()`] with underlying type
#'   \code{POSIXct}.
#' @param strip_attributes Logical; should semantic metadata attributes
#'   (label, unit, definition, namespace) be removed? Defaults to
#'   \code{FALSE}.
#'
#' @param ... Additional arguments passed to [base::as.POSIXct()].
#'
#' @return A `POSIXct` vector with timestamp values preserved.
#'
#' @examples
#' p <- defined(
#'   as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
#'   label = "Timestamp"
#' )
#'
#' # Recommended usage
#' as.POSIXct(p)
#'
#' # Explicit attribute stripping
#' as.POSIXct(p, strip_attributes = TRUE)
#'
#' @seealso
#'   [`as.Date()`],
#'   [`as_numeric()`], [`as_character()`], [`as_logical()`],
#'   [`defined()`]
#'
#' @export
as.POSIXct.haven_labelled_defined <- function(
    x,
    strip_attributes = FALSE,
    ...) {
  if (!inherits(x, "POSIXct")) {
    stop(
      "as.POSIXct.haven_labelled_defined() requires underlying POSIXct.",
      call. = FALSE
    )
  }

  if (strip_attributes) {
    attr(x, "label") <- NULL
    attr(x, "unit") <- NULL
    attr(x, "definition") <- NULL
    attr(x, "namespace") <- NULL
  }

  # Remove only the wrapper class
  class(x) <- setdiff(class(x), "haven_labelled_defined")

  x
}
