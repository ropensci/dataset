#' Coerce a defined POSIXct vector to a base R POSIXct
#'
#' @description
#' Coerces a [`haven_labelled_defined`] vector whose underlying type is
#' [`POSIXct`] into a base R `POSIXct` time vector.
#'
#' This method preserves both the timestamp values and the original time zone.
#' By default, semantic metadata is also retained.
#'
#' @details
#' Use \code{strip_attributes = TRUE} when flattening or preparing data for
#' external pipelines, but keep the default when working with defined
#' vectors directly.\cr
#' Base R's [`as.POSIXct()`] also works, as it dispatches to this method via
#' S3. Using this method directly is preferred when metadata preservation
#' matters.
#'
#' @param x A vector created with [`defined()`] with underlying type
#'   \code{POSIXct}.
#' @param tz a character string. The time zone specification to be used for
#' the conversion, if one is required. System-specific timezones
#' (see [`base::timezones()`],
#' but "" is the current time zone, and "GMT" is UTC (Universal Time,
#' Coordinated). Invalid values are most commonly treated as UTC, on
#' some platforms with a warning.
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
    tz = "",
    strip_attributes = TRUE,
    ...) {

  if (!inherits(x, "POSIXct")) {
    stop(
      "as.POSIXct.haven_labelled_defined() requires underlying POSIXct.",
      call. = FALSE
    )
  }

  # Save semantic metadata before we manipulate the object
  meta <- list(
    label      = attr(x, "label",      exact = TRUE),
    unit       = attr(x, "unit",       exact = TRUE),
    definition = attr(x, "definition", exact = TRUE),
    namespace  = attr(x, "namespace",  exact = TRUE)
  )

  # Drop only the wrapper class so base::as.POSIXct() can handle tz
  class(x) <- setdiff(class(x), "haven_labelled_defined")

  # If the user explicitly requests a time zone, delegate to base R
  if (!identical(tz, "")) {
    x <- base::as.POSIXct(x, tz = tz, ...)
  }

  if (strip_attributes) {
    attr(x, "label")      <- NULL
    attr(x, "unit")       <- NULL
    attr(x, "definition") <- NULL
    attr(x, "namespace")  <- NULL
  } else {
    # Restore metadata in case base::as.POSIXct() dropped anything
    for (nm in names(meta)) {
      if (!is.null(meta[[nm]])) {
        attr(x, nm) <- meta[[nm]]
      }
    }
  }

  x
}
