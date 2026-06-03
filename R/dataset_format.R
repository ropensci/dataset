#' Get or set the technical format of a dataset
#'
#' Adds or retrieves the optional `"format"` field of a dataset's bibentry.
#' This field is the dataset's technical/media type (e.g., a MIME type).
#'
#' @details
#' The format field corresponds to
#' [dct:format](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/format/)
#' in Dublin Core and to `format` in
#' [DataCite](https://schema.datacite.org/).
#' It is useful for indicating serialization such as `"text/csv"`,
#' `"application/parquet"`, or `"application/r-rds"`.
#'
#' If no format is set, this helper uses the package default
#' `"application/r-rds"`.
#'
#' @param x A semantically rich data frame created with [dataset_df()] or
#'   [as_dataset_df()].
#' @param value A length<U+2011>one character string specifying the format
#'   (e.g., `"text/csv"`). Use `NULL` to reset to the default.
#' @param overwrite Logical. Replace an existing non<U+2011>default value? If `FALSE`
#'   and a non<U+2011>default value already exists, a message is emitted and the value
#'   is kept. Defaults to `FALSE`.
#'
#' @return
#' The `"format"` (technical format) as a character string (length 1).
#' When assigning, the updated object `x` is returned invisibly.
#'
#' @examples
#' dataset_format(orange_df) <- "text/csv"
#' dataset_format(orange_df)
#'
#' # Reset to the package default
#' dataset_format(orange_df) <- NULL
#'
#' @family bibliographic helper functions
#' @importFrom assertthat assert_that
#' @export
dataset_format <- function(x) {
  assertthat::assert_that(
    is.dataset_df(x),
    msg = "dataset_format(x): x must be a dataset object created with dataset_df() or as_dataset_df()."
  )
  be <- get_bibentry(x)
  val <- be$format
  if (is.null(val)) "application/r-rds" else as.character(val)
}

#' @rdname dataset_format
#' @export
`dataset_format<-` <- function(x, overwrite = FALSE, value) {
  assertthat::assert_that(
    is.dataset_df(x),
    msg = "dataset_format(x) <- value: x must be a dataset object created with dataset_df() or as_dataset_df()."
  )
  assertthat::assert_that(
    is.null(value) || length(value) == 1,
    msg = "dataset_format(x) <- value: `value` must be length 1 or NULL."
  )

  be <- get_bibentry(x)

  if (is.null(value)) {
    be$format <- "application/r-rds"
    attr(x, "dataset_bibentry") <- be
    return(invisible(x))
  }

  # Treat these as non-user values we can safely replace without overwrite=TRUE
  is_default <- is.null(be$format) ||
    identical(be$format, "application/r-rds") ||
    identical(be$format, ":tba")

  if (is_default || overwrite) {
    be$format <- as.character(value)
  } else {
    message(
      "The dataset already has a format: ", be$format,
      ". Use overwrite = TRUE to replace it."
    )
  }

  attr(x, "dataset_bibentry") <- be
  invisible(x)
}
