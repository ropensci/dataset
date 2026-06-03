#' @title Get or set the dataset Description
#'
#' @description Get or set the optional `Description` property as an attribute
#'   on a dataset object.
#'
#' @details The `Description` is recommended for discovery in DataCite. It
#'   captures additional information that does not fit other metadata categories
#'   <U+2014> such as technical notes or dataset usage. It is a free-text field. See
#'   [dct:description](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/description/).
#'
#' @param x A dataset object created with [dataset_df()] or
#'   [as_dataset_df()].
#' @param value The new description, as a character string.
#' @param overwrite Logical. If `TRUE`, will overwrite any existing description.
#'   If `FALSE` (default), will warn and keep the existing description.
#'
#' @return The `Description` attribute as a character vector of length 1.
#'
#' @examples
#' description(orange_df)
#' description(orange_df, overwrite = TRUE) <- "This dataset records orange tree growth."
#' description(orange_df)
#'
#' @family bibliographic helper functions
#' @export

description <- function(x) {
  assert_that(is.dataset_df(x),
    msg = "description(x): x must be a dataset object created with dataset_df() or as_dataset_df()."
  )

  ds_bibentry <- get_bibentry(x)
  as.character(ds_bibentry$description)
}

#' @rdname description
#' @export
`description<-` <- function(x, overwrite = FALSE, value) {
  assert_that(is.dataset_df(x),
    msg = "description(x) <- value: x must be a dataset object created with dataset_df() or as_dataset_df()."
  )

  ds_bibentry <- get_bibentry(x)
  existing_description <- as.character(ds_bibentry$description)

  if (is.null(value)) {
    value <- ":unas"
  }

  if (overwrite ||
    length(existing_description) == 0 ||
    existing_description %in% c("", ":unas", ":tba")) {
    ds_bibentry$description <- as.character(value)
    attr(x, "dataset_bibentry") <- ds_bibentry
  } else {
    warning(
      "The dataset already has a description: ",
      existing_description, "."
    )
  }
  invisible(x)
}
